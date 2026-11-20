package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.RevokeFeaturedAppRightInstruction
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.DsoRules_CreateRevokeFeaturedAppRightInstruction
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.actionrequiringconfirmation.ARC_DsoRules
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.dsorules_actionrequiringconfirmation.SRARC_CreateRevokeFeaturedAppRightInstruction
import org.lfdecentralizedtrust.splice.config.ConfigTransforms.{
  ConfigurableApp,
  updateAutomationConfig,
}
import org.lfdecentralizedtrust.splice.console.{
  ValidatorAppBackendReference,
  WalletAppClientReference,
}
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTest,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.{
  ExecuteRevokeFeaturedAppRightInstructionTrigger,
  ExpireRevokeFeaturedAppRightInstructionTrigger,
}
import org.lfdecentralizedtrust.splice.util.{TimeTestUtil, WalletTestUtil}

@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_32
class RevokeFeaturedAppRightInstructionTimeBasedIntegrationTest
    extends IntegrationTest
    with WalletTestUtil
    with TimeTestUtil {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)
      .addConfigTransform((_, config) =>
        updateAutomationConfig(ConfigurableApp.Sv)(
          _.withPausedTrigger[ExecuteRevokeFeaturedAppRightInstructionTrigger]
            .withPausedTrigger[ExpireRevokeFeaturedAppRightInstructionTrigger]
        )(config)
      )

  "ExecuteRevokeFeaturedAppRightInstructionTrigger revokes the provider's featured app right" in {
    implicit env =>
      val providerParty = setupProviderWithFeaturedAppRight(bobWalletClient, bobValidatorBackend)

      actAndCheck(
        "SV votes to revoke the provider's featured app right",
        voteToRevokeFeaturedAppRight(providerParty),
      )(
        "the revoke instruction is created",
        _ => revokeInstructions() should not be empty,
      )

      actAndCheck(
        "the execute trigger runs",
        sv1Backend.dsoDelegateBasedAutomation
          .trigger[ExecuteRevokeFeaturedAppRightInstructionTrigger]
          .runOnce()
          .futureValue,
      )(
        "the featured app right and the instruction are archived",
        _ => {
          sv1ScanBackend.lookupFeaturedAppRight(providerParty) shouldBe empty
          revokeInstructions() shouldBe empty
        },
      )
  }

  "ExpireRevokeFeaturedAppRightInstructionTrigger expires an unexecuted instruction" in {
    implicit env =>
      val providerParty =
        setupProviderWithFeaturedAppRight(aliceWalletClient, aliceValidatorBackend)
      val voteRequestTimeout =
        sv1Backend.getDsoInfo().dsoRules.payload.config.voteRequestTimeout

      actAndCheck(
        "SV votes to revoke the provider's featured app right",
        voteToRevokeFeaturedAppRight(providerParty),
      )(
        "the revoke instruction is created",
        _ => revokeInstructions() should not be empty,
      )

      advanceTime(
        java.time.Duration.ofNanos(voteRequestTimeout.microseconds * 1000).plusMinutes(1)
      )

      actAndCheck(
        "the expire trigger runs",
        sv1Backend.dsoDelegateBasedAutomation
          .trigger[ExpireRevokeFeaturedAppRightInstructionTrigger]
          .runOnce()
          .futureValue,
      )(
        "the instruction is archived and the featured app right is kept",
        _ => {
          revokeInstructions() shouldBe empty
          sv1ScanBackend.lookupFeaturedAppRight(providerParty) should not be empty
        },
      )
  }

  private def revokeInstructions()(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.appState.dsoStore.multiDomainAcsStore
      .listContracts(RevokeFeaturedAppRightInstruction.COMPANION)
      .futureValue

  private def setupProviderWithFeaturedAppRight(
      walletClient: WalletAppClientReference,
      validatorBackend: ValidatorAppBackendReference,
  )(implicit env: SpliceTestConsoleEnvironment): PartyId = {
    val providerParty = onboardWalletUser(walletClient, validatorBackend)
    actAndCheck(
      "DSO creates the provider's FeaturedAppRight",
      sv1Backend.participantClientWithAdminToken.ledger_api_extensions.commands
        .submitWithResult(
          userId = sv1Backend.config.ledgerApiUser,
          actAs = Seq(dsoParty),
          readAs = Seq.empty,
          update = new FeaturedAppRight(
            dsoParty.toProtoPrimitive,
            providerParty.toProtoPrimitive,
            java.util.Optional.empty(),
            java.util.Optional.empty(),
            java.util.Optional.empty(),
            java.util.Optional.empty(),
          ).create,
        ),
    )(
      "the provider's featured app right is visible in scan",
      _ => sv1ScanBackend.lookupFeaturedAppRight(providerParty) should not be empty,
    )
    providerParty
  }

  private def voteToRevokeFeaturedAppRight(providerParty: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): Unit = {
    val action = new ARC_DsoRules(
      new SRARC_CreateRevokeFeaturedAppRightInstruction(
        new DsoRules_CreateRevokeFeaturedAppRightInstruction(providerParty.toProtoPrimitive)
      )
    )
    sv1Backend.createVoteRequest(
      sv1Backend.getDsoInfo().svParty.toProtoPrimitive,
      action,
      "url",
      "revoke featured app right",
      sv1Backend.getDsoInfo().dsoRules.payload.config.voteRequestTimeout,
      None,
    )
  }
}
