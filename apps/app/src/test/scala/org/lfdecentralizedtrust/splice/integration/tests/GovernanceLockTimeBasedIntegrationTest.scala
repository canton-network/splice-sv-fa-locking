// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.IntegrationTestWithIsolatedEnvironment
import org.lfdecentralizedtrust.splice.util.{TimeTestUtil, WalletTestUtil}

class GovernanceLockTimeBasedIntegrationTest
    extends IntegrationTestWithIsolatedEnvironment
    with WalletTestUtil
    with TimeTestUtil
    with TokenStandardTest {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)

  private def matchSVKind(kind: governancelock.GovernanceLockKind): Unit =
    kind match {
      case _: governancelock.governancelockkind.GLK_SuperValidatorRightsOwner =>
      case other => fail(s"Expected GLK_SuperValidatorRightsOwner kind, found: $other")
    }

  "SV GovernanceLock created via TSv1 compatibility interface can be unlocked into a VestingLock" in {
    implicit env =>
      val lockAmount = BigDecimal(10000)

      // Setup alice as the lock owner
      val ownerParty = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      val owner = RichPartyId.local(ownerParty)

      aliceWalletClient.tap(lockAmount)

      // Create the governance lock via a transfer to the magic party
      val governanceLockCid = createGovernanceLockViaTokenStandard(
        aliceValidatorBackend.participantClientWithAdminToken,
        owner,
        superValidatorLockMagicParty,
        lockSubject = "sv1",
        amount = lockAmount,
      )

      clue("the GovernanceLock has the correct kind") {
        val governanceLock =
          aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
            .filterJava(governancelock.GovernanceLock.COMPANION)(
              ownerParty,
              predicate = _.id.contractId == governanceLockCid.contractId,
            )
            .loneElement
            .data
        matchSVKind(governanceLock.specification.kind)
      }

      clue("the GovernanceLock is as a pending TransferInstruction to the magic party") {
        val (cid, view) = listTransferInstructions(
          aliceValidatorBackend.participantClientWithAdminToken,
          ownerParty,
        ).loneElement
        cid.contractId shouldBe governanceLockCid.contractId
        view.transfer.sender shouldBe ownerParty.toProtoPrimitive
        view.transfer.receiver shouldBe superValidatorLockMagicParty.toProtoPrimitive
        BigDecimal(view.transfer.amount) shouldBe lockAmount
      }

      clue("Scan serves a withdraw choice context for the GovernanceLock") {
        sv1ScanBackend
          .getTransferInstructionWithdrawContext(governanceLockCid)
          .disclosedContracts should not be empty
      }

      // Advance sim-time past the lock's createdAt time
      advanceTimeAndWaitForRoundOpening

      // Withdraw the governance lock; it's relocked as a VestingLock and funds remain locked
      val (_, vestingLockCid) = actAndCheck(
        "the owner withdraws the GovernanceLock",
        withdrawTransferInstruction(
          aliceValidatorBackend.participantClientWithAdminToken,
          owner,
          governanceLockCid,
        ),
      )(
        "a VestingLock is now the pending TransferInstruction to the magic party",
        _ => {
          val (cid, view) = listTransferInstructions(
            aliceValidatorBackend.participantClientWithAdminToken,
            ownerParty,
          ).loneElement
          val vestingLock =
            aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
              .filterJava(governancelock.VestingLock.COMPANION)(
                ownerParty,
                predicate = _.id.contractId == cid.contractId,
              )
              .loneElement
              .data
          cid.contractId should not be governanceLockCid.contractId
          view.transfer.sender shouldBe ownerParty.toProtoPrimitive
          view.transfer.receiver shouldBe superValidatorLockMagicParty.toProtoPrimitive
          matchSVKind(vestingLock.specification.kind)
          cid
        },
      )

      clue("Scan serves a withdraw choice context for the VestingLock") {
        sv1ScanBackend
          .getTransferInstructionWithdrawContext(vestingLockCid)
          .disclosedContracts should not be empty
      }

      clue("the amulet remains locked while vesting") {
        aliceWalletClient.balance().lockedQty should beAround(lockAmount)
      }
  }
}
