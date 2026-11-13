// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.config.RequireTypes.NonNegativeNumeric
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.FeaturedAppUnderlockInstruction
import org.lfdecentralizedtrust.splice.config.{ConfigTransform, ConfigTransforms}
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  SpliceTestConsoleEnvironment,
  TestCommon,
}
import org.lfdecentralizedtrust.splice.sv.config.InitialGovernanceLockManagementConfig

trait FeaturedAppUnderlockTestUtil extends TestCommon {

  protected def withFeaturedAppUnderlockEnforcement(
      basicLockThreshold: Option[BigDecimal] = None,
      gracePeriod: Option[NonNegativeFiniteDuration] = None,
  ): ConfigTransform =
    ConfigTransforms.updateAllSvAppFoundDsoConfigs_(
      _.copy(
        initialGovernanceLockManagementConfig = Some(
          InitialGovernanceLockManagementConfig(
            featuredAppUnderlockEnforcementEnabled = Some(true),
            featuredAppBasicLockThreshold = basicLockThreshold.map(NonNegativeNumeric.tryCreate(_)),
            featuredAppUnderlockGracePeriod = gracePeriod,
          )
        )
      )
    )

  protected def underlockInstructionsFor(
      provider: PartyId
  )(implicit env: SpliceTestConsoleEnvironment): Seq[FeaturedAppUnderlockInstruction.Contract] =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(FeaturedAppUnderlockInstruction.COMPANION)(
        dsoParty,
        _.data.provider == provider.toProtoPrimitive,
      )

  protected def featuredAppRightsFor(
      provider: PartyId
  )(implicit env: SpliceTestConsoleEnvironment): Seq[FeaturedAppRight.Contract] =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(FeaturedAppRight.COMPANION)(
        dsoParty,
        _.data.provider == provider.toProtoPrimitive,
      )

  /** The provider's only featured app right. */
  protected def featuredAppRightOf(provider: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): FeaturedAppRight.Contract =
    featuredAppRightsFor(provider).loneElement

  protected def isSuspended(provider: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): Boolean =
    featuredAppRightsFor(provider).loneElement.data.underlockPermanentAt.isPresent
}
