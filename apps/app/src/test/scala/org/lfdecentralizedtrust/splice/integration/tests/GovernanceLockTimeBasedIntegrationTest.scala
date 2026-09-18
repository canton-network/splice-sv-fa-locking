// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import org.lfdecentralizedtrust.splice.codegen.java.splice.{amulet as amuletCodegen, governancelock}
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.IntegrationTestWithIsolatedEnvironment
import org.lfdecentralizedtrust.splice.util.{TimeTestUtil, WalletTestUtil}

import java.time.Duration

class GovernanceLockTimeBasedIntegrationTest
    extends IntegrationTestWithIsolatedEnvironment
    with WalletTestUtil
    with TimeTestUtil
    with TokenStandardTest {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppFoundDsoConfigs_(
          _.copy(
            // Shorten the SV vesting period and search time granularity so we can test partial and
            // full withdrawal while only advancing the clock a couple minutes
            initialGovernanceLockSuperValidatorLockVestingDuration =
              Some(NonNegativeFiniteDuration.ofSeconds(5)),
            initialGovernanceLockSearchTimeGranularity = Some(NonNegativeFiniteDuration.ofMicros(1)),
          )
        )(config)
      )

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

      // Advance sim-time past the lock's requestedAt time
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

      // Advance 2.5 seconds into the 5 second vesting period and withdraw
      advanceTime(Duration.ofMillis(2500))

      val (_, (remainingVestingLockCid, remainingVestingAmount)) = actAndCheck(
        "the owner withdraws the VestingLock mid-vesting",
        withdrawTransferInstruction(
          aliceValidatorBackend.participantClientWithAdminToken,
          owner,
          vestingLockCid,
        ),
      )(
        "a new VestingLock remains with half of the total vesting amount",
        _ => {
          val (cid, view) = listTransferInstructions(
            aliceValidatorBackend.participantClientWithAdminToken,
            ownerParty,
          ).loneElement
          val remainingVestingLock =
            aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
              .filterJava(governancelock.VestingLock.COMPANION)(
                ownerParty,
                predicate = _.id.contractId == cid.contractId,
              )
              .loneElement
              .data
          cid.contractId should not be vestingLockCid.contractId
          view.transfer.sender shouldBe ownerParty.toProtoPrimitive
          view.transfer.receiver shouldBe superValidatorLockMagicParty.toProtoPrimitive
          matchSVKind(remainingVestingLock.specification.kind)
          val remainingVestingAmount = BigDecimal(remainingVestingLock.vestingAmount)
          remainingVestingAmount should beAround(lockAmount / 2)
          aliceWalletClient.balance().lockedQty should beAround(lockAmount / 2)
          (cid, remainingVestingAmount)
        },
      )

      // Advance past the end of the vesting period and withdraw
      advanceTime(Duration.ofSeconds(5))

      actAndCheck(
        "the owner withdraws the fully-vested VestingLock",
        withdrawTransferInstruction(
          aliceValidatorBackend.participantClientWithAdminToken,
          owner,
          remainingVestingLockCid,
        ),
      )(
        "the VestingLock is archived and no pending TransferInstruction remains",
        _ => {
          listTransferInstructions(
            aliceValidatorBackend.participantClientWithAdminToken,
            ownerParty,
          ) shouldBe empty
          aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
            .filterJava(governancelock.VestingLock.COMPANION)(ownerParty, _ => true) shouldBe empty
        },
      )

      clue("the only LockedAmulet of the owner is the expired LockedAmulet of the vesting lock") {
        val now = getLedgerTime.toInstant
        val remainingLock =
          aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
            .filterJava(amuletCodegen.LockedAmulet.COMPANION)(ownerParty, _ => true)
            .loneElement
            .data
        remainingLock.lock.expiresAt.isAfter(now) shouldBe false
        BigDecimal(remainingLock.amulet.amount.initialAmount) should beAround(
          remainingVestingAmount
        )
      }
  }
}
