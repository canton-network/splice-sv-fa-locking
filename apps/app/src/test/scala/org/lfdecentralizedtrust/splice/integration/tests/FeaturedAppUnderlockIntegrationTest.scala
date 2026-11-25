package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.FeaturedAppUnderlockInstruction
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.config.ConfigTransforms.{
  ConfigurableApp,
  updateAutomationConfig,
}
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTest,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.automation.confirmation.FeaturedAppUnderlockStatusUpdateTrigger
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.FeaturedAppPermanentUnderlockTrigger
import org.lfdecentralizedtrust.splice.util.*

import java.time.Instant
import scala.concurrent.duration.*

@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_32
class FeaturedAppUnderlockIntegrationTest
    extends IntegrationTest
    with WalletTestUtil
    with GovernanceLockTest
    with FeaturedAppUnderlockTestUtil
    with TriggerTestUtil {

  // The smallest amount a lock can have, so a single lock is enough to meet the threshold
  private val basicLockThreshold = minimumLockAmount

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1Sv(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        withFeaturedAppUnderlockEnforcement(
          basicLockThreshold = Some(basicLockThreshold),
          gracePeriod = Some(NonNegativeFiniteDuration.ofSeconds(5)),
        )(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppConfigs_(
          _.copy(
            featuredAppUnderlockStatusUpdateInterval = NonNegativeFiniteDuration.ofSeconds(1)
          )
        )(config)
      )
      // Otherwise it archives charlie's suspended right before charlie locks enough
      .addConfigTransforms((_, config) =>
        updateAutomationConfig(ConfigurableApp.Sv)(
          _.withPausedTrigger[FeaturedAppPermanentUnderlockTrigger]
        )(config)
      )

  "create a single underlock instruction per featured app provider without locks" in {
    implicit env =>
      val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      val bob = onboardWalletUser(bobWalletClient, bobValidatorBackend)

      val (_, instructionsSeen) = actAndCheck(
        "alice and bob grant themselves a FeaturedAppRight without locking anything",
        Seq(aliceWalletClient, bobWalletClient).foreach(grantFeaturedAppRight),
      )(
        "the SVs record alice and bob as underlocked",
        _ =>
          Seq(alice, bob).map { provider =>
            // The instruction is either awaiting execution, or already executed and suspended the right
            val instructions = underlockInstructionsFor(provider)
            instructions.foreach(_.data.underlocked shouldBe true)
            if (instructions.isEmpty) isSuspended(provider) shouldBe true
            provider -> instructions.size
          },
      )

      clue("no duplicate instructions are created") {
        always(10.seconds, pollIntervalMs = 1000) {
          instructionsSeen.foreach { case (provider, seen) =>
            val numInstructions = underlockInstructionsFor(provider).size
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
      grantFeaturedAppRight(charlieWalletClient),
    )(
      "charlie's right is suspended",
      _ => {
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
        isSuspended(charlie) shouldBe false
      },
    )
  }

  "archive a suspended featured app right once its grace period has passed" in { implicit env =>
    val provider = bobValidatorBackend.getValidatorPartyId()
    val trigger =
      sv1Backend.dsoDelegateBasedAutomation.trigger[FeaturedAppPermanentUnderlockTrigger]

    actAndCheck(
      "bob's validator grants itself a FeaturedAppRight without locking anything",
      eventuallySucceeds() {
        bobValidatorWalletClient.selfGrantFeaturedAppRight()
      },
    )(
      "the right is suspended",
      _ => {
        isSuspended(provider) shouldBe true
      },
    )

    clue("the trigger archives the right once its grace period has passed") {
      // The trigger also archives the rights suspended in earlier tests, so
      // one run may not cover this right.
      eventually() {
        trigger.runOnce().futureValue
        featuredAppRightsFor(provider) shouldBe empty
      }
    }
  }

  // TODO(canton-network/splice-sv-fa-locking#205): pause the trigger that
  // expires underlock instructions here, once it exists.
  "execute an expired instruction that has not been archived yet" in { implicit env =>
    val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
    withStatusUpdateTriggerPaused {
      grantFeaturedAppRight(aliceWalletClient)
      actAndCheck(
        "the DSO party creates an underlock instruction for alice " +
          "that expired a minute ago",
        createUnderlockInstruction(alice, Instant.now().minusSeconds(60)),
      )(
        "the instruction is archived and alice's right is suspended",
        _ => {
          underlockInstructionsFor(alice) shouldBe empty
          isSuspended(alice) shouldBe true
        },
      )
    }
  }

  "execute an instruction only when its provider has a featured app right" in { implicit env =>
    val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
    val bob = onboardWalletUser(bobWalletClient, bobValidatorBackend)
    val expiresAt = Instant.now().plusSeconds(3600)
    withStatusUpdateTriggerPaused {
      grantFeaturedAppRight(aliceWalletClient)
      // The SV store ingests in ledger order, so once the trigger has
      // executed alice's instruction, it has also seen bob's.
      actAndCheck(
        "the DSO party creates underlock instructions for bob, who has " +
          "no right, and then for alice", {
          createUnderlockInstruction(bob, expiresAt)
          createUnderlockInstruction(alice, expiresAt)
        },
      )(
        "alice's instruction is executed and bob's is left",
        _ => {
          underlockInstructionsFor(alice) shouldBe empty
          isSuspended(alice) shouldBe true
          underlockInstructionsFor(bob) should have size 1
        },
      )
      actAndCheck(
        "bob grants himself a featured app right",
        grantFeaturedAppRight(bobWalletClient),
      )(
        "bob's instruction is executed",
        _ => {
          underlockInstructionsFor(bob) shouldBe empty
          isSuspended(bob) shouldBe true
        },
      )
    }
  }

  /** Runs `block` with the status update trigger paused to ensure that the
    * status update trigger doesn't create underlock instructions outside of
    * this test.
    */
  private def withStatusUpdateTriggerPaused[T](block: => T)(implicit
      env: SpliceTestConsoleEnvironment
  ): T =
    setTriggersWithin(
      triggersToPauseAtStart = Seq(
        sv1Backend.dsoAutomation
          .trigger[FeaturedAppUnderlockStatusUpdateTrigger]
      )
    )(block)

  private def createUnderlockInstruction(
      provider: PartyId,
      expiresAt: Instant,
  )(implicit env: SpliceTestConsoleEnvironment): Unit = {
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.commands
      .submitWithResult(
        userId = sv1Backend.config.ledgerApiUser,
        actAs = Seq(dsoParty),
        readAs = Seq.empty,
        update = new FeaturedAppUnderlockInstruction(
          dsoParty.toProtoPrimitive,
          provider.toProtoPrimitive,
          true,
          expiresAt,
        ).create,
      )
    ()
  }
}
