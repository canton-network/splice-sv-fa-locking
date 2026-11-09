package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.topology.PartyId
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

  "create a single underlock instruction for a featured app provider without locks" in {
    implicit env =>
      val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)

      actAndCheck(
        "alice grants herself a FeaturedAppRight without locking anything",
        eventuallySucceeds() {
          aliceWalletClient.selfGrantFeaturedAppRight()
        },
      )(
        "the SVs confirm an underlock instruction for alice",
        _ => {
          val instruction = instructionsFor(alice).loneElement
          instruction.underlocked shouldBe true
        },
      )

      // Nothing executes the instruction in this test, so alice's right still records no
      // underlock. The trigger must not create further instructions while one is pending.
      clue("no further instructions are created while one awaits execution") {
        always(10.seconds, pollIntervalMs = 1000) {
          instructionsFor(alice) should have size 1
        }
      }
  }
}
