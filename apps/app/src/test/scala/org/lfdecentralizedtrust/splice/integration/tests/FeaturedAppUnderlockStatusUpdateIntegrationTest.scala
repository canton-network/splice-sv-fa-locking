package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.FeaturedAppUnderlockInstruction
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTest,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.config.InitialGovernanceLockManagementConfig
import org.lfdecentralizedtrust.splice.util.*

import scala.concurrent.duration.*

@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_32
class FeaturedAppUnderlockStatusUpdateIntegrationTest extends IntegrationTest with WalletTestUtil {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1Sv(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppFoundDsoConfigs_(
          _.copy(
            initialGovernanceLockManagementConfig = Some(
              InitialGovernanceLockManagementConfig(
                featuredAppUnderlockEnforcementEnabled = Some(true)
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
  )(implicit env: SpliceTestConsoleEnvironment): Seq[FeaturedAppUnderlockInstruction] =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(FeaturedAppUnderlockInstruction.COMPANION)(
        dsoParty,
        _.data.provider == provider.toProtoPrimitive,
      )
      .map(_.data)

  private def featuredAppRightsFor(
      provider: PartyId
  )(implicit env: SpliceTestConsoleEnvironment): Seq[FeaturedAppRight] =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(FeaturedAppRight.COMPANION)(
        dsoParty,
        _.data.provider == provider.toProtoPrimitive,
      )
      .map(_.data)

  "create a single underlock instruction for a featured app provider without locks" in {
    implicit env =>
      val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)

      actAndCheck(
        "alice grants herself a FeaturedAppRight without locking anything",
        eventuallySucceeds() {
          aliceWalletClient.selfGrantFeaturedAppRight()
        },
      )(
        "the SVs record alice as underlocked",
        _ => {
          // The instruction is either awaiting execution, or already executed and suspended the right
          val instructions = instructionsFor(alice)
          instructions.foreach(_.underlocked shouldBe true)
          if (instructions.isEmpty)
            featuredAppRightsFor(alice).map(_.underlockPermanentAt.isPresent) shouldBe Seq(true)
        },
      )

      clue("no duplicate instructions are created") {
        always(10.seconds, pollIntervalMs = 1000) {
          instructionsFor(alice).size should be <= 1
        }
      }
  }
}
