// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.IntegrationTest
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.FeaturedAppPermanentUnderlockTrigger
import org.lfdecentralizedtrust.splice.util.*

@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_32
class FeaturedAppPermanentUnderlockIntegrationTest
    extends IntegrationTest
    with WalletTestUtil
    with FeaturedAppUnderlockTest {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1Sv(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        withFeaturedAppUnderlockEnforcement(
          gracePeriod = Some(NonNegativeFiniteDuration.ofSeconds(5))
        )(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppConfigs_(
          _.copy(
            featuredAppUnderlockStatusUpdateInterval = NonNegativeFiniteDuration.ofSeconds(1)
          )
        )(config)
      )

  "archive a suspended featured app right once its grace period has passed" in { implicit env =>
    val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
    // Paused by default in tests, so it only runs when we call `runOnce`
    val trigger =
      sv1Backend.dsoDelegateBasedAutomation.trigger[FeaturedAppPermanentUnderlockTrigger]

    actAndCheck(
      "alice self-grants a FeaturedAppRight without locking anything",
      eventuallySucceeds() {
        aliceWalletClient.selfGrantFeaturedAppRight()
      },
    )(
      "alice's right is suspended",
      _ => {
        executeInstructionsFor(alice)
        isSuspended(alice) shouldBe true
      },
    )

    clue("the trigger archives alice's right once its grace period has passed") {
      eventually()(trigger.runOnce().futureValue shouldBe true)
      eventually()(featuredAppRightsFor(alice) shouldBe empty)
    }
  }
}
