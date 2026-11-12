package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.config.RequireTypes.NonNegativeNumeric
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.FeaturedAppUnderlockInstruction
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.DsoRules
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTest,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.config.InitialGovernanceLockManagementConfig
import org.lfdecentralizedtrust.splice.util.*

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_32
class FeaturedAppUnderlockStatusUpdateIntegrationTest
    extends IntegrationTest
    with WalletTestUtil
    with GovernanceLockTest {

  // The smallest amount a lock can have, so a single lock is enough to meet the threshold
  private val basicLockThreshold = minimumLockAmount

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1Sv(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppFoundDsoConfigs_(
          _.copy(
            initialGovernanceLockManagementConfig = Some(
              InitialGovernanceLockManagementConfig(
                featuredAppUnderlockEnforcementEnabled = Some(true),
                featuredAppBasicLockThreshold = Some(NonNegativeNumeric.tryCreate(basicLockThreshold)),
              )
            )
          )
        )(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppConfigs_(
          _.copy(
            featuredAppUnderlockStatusUpdateInterval = NonNegativeFiniteDuration.ofSeconds(1)
          )
        )(config)
      )

  private def instructionsFor(
      provider: PartyId
  )(implicit env: SpliceTestConsoleEnvironment): Seq[FeaturedAppUnderlockInstruction.Contract] =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(FeaturedAppUnderlockInstruction.COMPANION)(
        dsoParty,
        _.data.provider == provider.toProtoPrimitive,
      )

  private def featuredAppRightsFor(
      provider: PartyId
  )(implicit env: SpliceTestConsoleEnvironment): Seq[FeaturedAppRight.Contract] =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(FeaturedAppRight.COMPANION)(
        dsoParty,
        _.data.provider == provider.toProtoPrimitive,
      )

  private def isSuspended(provider: PartyId)(implicit env: SpliceTestConsoleEnvironment): Boolean =
    featuredAppRightsFor(provider).loneElement.data.underlockPermanentAt.isPresent

  // TODO(canton-network/splice-sv-fa-locking#204): Remove once ExecuteFeaturedAppUnderlockInstructionTrigger
  // executes instructions, as this would race with it.
  private def executeInstructionsFor(
      provider: PartyId
  )(implicit env: SpliceTestConsoleEnvironment): Unit = {
    val participant = sv1Backend.participantClientWithAdminToken
    val sv1Party = sv1Backend.getDsoInfo().svParty
    instructionsFor(provider).foreach { instruction =>
      val dsoRulesCid =
        participant.ledger_api_extensions.acs.filterJava(DsoRules.COMPANION)(dsoParty).loneElement.id
      participant.ledger_api_extensions.commands.submitJava(
        actAs = Seq(sv1Party),
        readAs = Seq(dsoParty),
        commands = dsoRulesCid
          .exerciseDsoRules_ExecuteFeaturedAppUnderlockInstruction(
            instruction.id,
            featuredAppRightsFor(provider).loneElement.id,
            sv1Party.toProtoPrimitive,
          )
          .commands
          .asScala
          .toSeq,
      )
    }
  }

  "create a single underlock instruction per featured app provider without locks" in {
    implicit env =>
      val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      val bob = onboardWalletUser(bobWalletClient, bobValidatorBackend)

      val (_, instructionsSeen) = actAndCheck(
        "alice and bob grant themselves a FeaturedAppRight without locking anything",
        Seq(aliceWalletClient, bobWalletClient).foreach(client =>
          eventuallySucceeds() {
            client.selfGrantFeaturedAppRight()
          }
        ),
      )(
        "the SVs record alice and bob as underlocked",
        _ =>
          Seq(alice, bob).map { provider =>
            // The instruction is either awaiting execution, or already executed and suspended the right
            val instructions = instructionsFor(provider)
            instructions.foreach(_.data.underlocked shouldBe true)
            if (instructions.isEmpty) isSuspended(provider) shouldBe true
            provider -> instructions.size
          },
      )

      clue("no duplicate instructions are created") {
        always(10.seconds, pollIntervalMs = 1000) {
          instructionsSeen.foreach { case (provider, seen) =>
            val numInstructions = instructionsFor(provider).size
            // Once executed, the right records the underlock, so no new instruction may appear
            if (seen == 0) numInstructions shouldBe 0 else numInstructions should be <= 1
          }
        }
      }
  }

  "lift the suspension once a provider locks enough" in { implicit env =>
    val charlie = onboardWalletUser(charlieWalletClient, aliceValidatorBackend)
    charlieWalletClient.tap(basicLockThreshold)

    actAndCheck(
      "charlie grants himself a FeaturedAppRight without locking anything",
      eventuallySucceeds() {
        charlieWalletClient.selfGrantFeaturedAppRight()
      },
    )(
      "charlie's right is suspended",
      _ => {
        executeInstructionsFor(charlie)
        isSuspended(charlie) shouldBe true
      },
    )

    actAndCheck(
      "charlie locks enough for a basic featured app",
      // The lock starts out provisional, and is converted to a full lock as charlie has a right
      tsv1Operations.createGovernanceLock(
        aliceValidatorBackend.participantClientWithAdminToken,
        RichPartyId.local(charlie),
        lockKind = ProvisionalFeaturedAppLock,
        lockSubject = charlie.toProtoPrimitive,
        amount = basicLockThreshold,
      ),
    )(
      "charlie's suspension is lifted",
      _ => {
        executeInstructionsFor(charlie)
        isSuspended(charlie) shouldBe false
      },
    )
  }
}
