// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import org.lfdecentralizedtrust.splice.config.ConfigTransforms.{
  ConfigurableApp,
  updateAutomationConfig,
}
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.IntegrationTest
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.MergeDuplicateFeaturedAppRightsTrigger
import org.lfdecentralizedtrust.splice.util.WalletTestUtil

@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_32
class MergeDuplicateFeaturedAppRightsIntegrationTest
    extends IntegrationTest
    with WalletTestUtil
    with FeaturedAppUnderlockTestUtil {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1Sv(this.getClass.getSimpleName)
      .addConfigTransform((_, config) =>
        updateAutomationConfig(ConfigurableApp.Sv)(
          _.withPausedTrigger[MergeDuplicateFeaturedAppRightsTrigger]
        )(config)
      )

  "MergeDuplicateFeaturedAppRightsTrigger merges a provider's featured app rights into one" in {
    implicit env =>
      val provider = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      grantFeaturedAppRight(aliceWalletClient)
      grantFeaturedAppRight(aliceWalletClient)
      eventually()(featuredAppRightsFor(provider) should have size 2)

      clue("the merge trigger merges alice's rights into one") {
        eventually()(
          loggerFactory.assertLogs(
            sv1Backend.dsoDelegateBasedAutomation
              .trigger[MergeDuplicateFeaturedAppRightsTrigger]
              .runOnce()
              .futureValue shouldBe true,
            _.warningMessage should include("FeaturedAppRight contracts"),
          )
        )
        eventually()(featuredAppRightsFor(provider) should have size 1)
      }
  }
}
