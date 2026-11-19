// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.FeaturedAppUnderlockInstruction
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  SpliceTestConsoleEnvironment,
  TestCommon,
}

trait FeaturedAppUnderlockTestUtil extends TestCommon {

  /** The provider's underlock instructions that await execution. */
  protected def underlockInstructionsFor(provider: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): Seq[FeaturedAppUnderlockInstruction.Contract] =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(FeaturedAppUnderlockInstruction.COMPANION)(
        dsoParty,
        _.data.provider == provider.toProtoPrimitive,
      )

  /** The provider's only featured app right. */
  protected def featuredAppRightOf(provider: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): FeaturedAppRight.Contract =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(FeaturedAppRight.COMPANION)(
        dsoParty,
        _.data.provider == provider.toProtoPrimitive,
      )
      .loneElement

  /** Whether the provider's right is suspended for being underlocked. */
  protected def isSuspended(provider: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): Boolean =
    featuredAppRightOf(provider).data.underlockPermanentAt.isPresent
}
