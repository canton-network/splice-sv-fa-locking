// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.logging.SuppressionRule
import org.lfdecentralizedtrust.splice.config.ConfigTransforms.{
  ConfigurableApp,
  updateAutomationConfig,
}
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.IntegrationTest
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.MergeDuplicateFeaturedAppRightsTrigger
import org.lfdecentralizedtrust.splice.util.WalletTestUtil
import org.slf4j.event.Level

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

  "MergeDuplicateFeaturedAppRightsTrigger merges each provider's featured app rights into one" in {
    implicit env =>
      val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      val bob = onboardWalletUser(bobWalletClient, bobValidatorBackend)
      (1 to 3).foreach(_ => grantFeaturedAppRight(aliceWalletClient))
      (1 to 2).foreach(_ => grantFeaturedAppRight(bobWalletClient))
      eventually() {
        featuredAppRightsFor(alice) should have size 3
        featuredAppRightsFor(bob) should have size 2
      }

      clue("the merge trigger leaves alice and bob with one featured app right each") {
        loggerFactory.assertLogsSeq(SuppressionRule.LevelAndAbove(Level.WARN))(
          eventually() {
            sv1Backend.dsoDelegateBasedAutomation
              .trigger[MergeDuplicateFeaturedAppRightsTrigger]
              .runOnce()
              .futureValue
            featuredAppRightsFor(alice) should have size 1
            featuredAppRightsFor(bob) should have size 1
          },
          entries => {
            forAll(entries)(_.warningMessage should include("FeaturedAppRight contracts"))
            forAll(Seq(alice, bob))(provider =>
              atLeast(1, entries.map(_.warningMessage)) should include(provider.uid.identifier.str)
            )
          },
        )
      }
  }
}
