package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.RevokeFeaturedAppRightInstruction
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.DsoRules_CreateRevokeFeaturedAppRightInstruction
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.actionrequiringconfirmation.ARC_DsoRules
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.dsorules_actionrequiringconfirmation.SRARC_CreateRevokeFeaturedAppRightInstruction
import org.lfdecentralizedtrust.splice.config.ConfigTransforms.{
  ConfigurableApp,
  updateAutomationConfig,
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
import org.lfdecentralizedtrust.splice.util.{SpliceUtil, TimeTestUtil, WalletTestUtil}

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
      val providerParty = onboardWalletUser(bobWalletClient, bobValidatorBackend)
      grantFeaturedAppRight(bobWalletClient)
      createRevokeInstruction(providerParty)

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
      val providerParty = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      grantFeaturedAppRight(aliceWalletClient)
      createRevokeInstruction(providerParty)

      advanceTime(
        SpliceUtil
          .relTimeToDuration(sv1Backend.getDsoInfo().dsoRules.payload.config.voteRequestTimeout)
          .plusMinutes(1)
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
          sv1ScanBackend.lookupFeaturedAppRight(providerParty) should not be empty
          revokeInstructions() shouldBe empty
        },
      )
  }

  private def revokeInstructions()(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.appState.dsoStore.multiDomainAcsStore
      .listContracts(RevokeFeaturedAppRightInstruction.COMPANION)
      .futureValue

  private def createRevokeInstruction(providerParty: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): Unit = {
    val dsoInfo = sv1Backend.getDsoInfo()
    actAndCheck(
      "SV votes to revoke the provider's featured app right",
      sv1Backend.createVoteRequest(
        dsoInfo.svParty.toProtoPrimitive,
        new ARC_DsoRules(
          new SRARC_CreateRevokeFeaturedAppRightInstruction(
            new DsoRules_CreateRevokeFeaturedAppRightInstruction(providerParty.toProtoPrimitive)
          )
        ),
        "url",
        "revoke featured app right",
        dsoInfo.dsoRules.payload.config.voteRequestTimeout,
        None,
      ),
    )(
      "the revoke instruction is created",
      _ => revokeInstructions() should not be empty,
    )
  }
}
