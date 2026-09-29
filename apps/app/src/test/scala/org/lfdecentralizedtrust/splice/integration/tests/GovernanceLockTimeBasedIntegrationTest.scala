// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.daml.ledger.javaapi
import com.daml.ledger.javaapi.data.codegen.ContractId
import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.{amulet as amuletCodegen, governancelock}
import org.lfdecentralizedtrust.splice.codegen.java.splice.api.token.{
  allocationv2,
  transferinstructionv1,
}
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTestWithIsolatedEnvironment,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.config.InitialGovernanceLockConfig
import org.lfdecentralizedtrust.splice.util.{TimeTestUtil, TokenStandardMetadata, WalletTestUtil}
import org.lfdecentralizedtrust.splice.wallet.store.{BalanceChangeTxLogEntry, TxLogEntry}
import org.scalatest.Assertion

import java.time.Duration
import java.time.temporal.ChronoUnit
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

class GovernanceLockTimeBasedIntegrationTest
    extends IntegrationTestWithIsolatedEnvironment
    with WalletTestUtil
    with TimeTestUtil
    with WalletTxLogTestUtil
    with GovernanceLockTokenStandardTest {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppFoundDsoConfigs_(
          _.copy(
            // Shorten the SV vesting period so we can test partial and full withdrawal while only
            // advancing the clock a couple minutes
            initialGovernanceLockConfig = Some(
              InitialGovernanceLockConfig(
                superValidatorLockVestingDuration = Some(NonNegativeFiniteDuration.ofMinutes(2))
              )
            )
          )
        )(config)
      )

  private val lockAmount = BigDecimal(10000)
  private val expectedRemainingLockAmount = lockAmount / 4 * 3
  private val lockParty = superValidatorLockMagicParty
  private val lockSubject = "sv1"

  private def checkLockKind(kind: governancelock.GovernanceLockKind): Assertion =
    kind shouldBe a[governancelock.governancelockkind.GLK_SuperValidatorRightsOwner]

  private def testTokenStandardCompat[CId <: ContractId[?], View](
      ops: GovernanceLockTokenStandardOps[CId, View]
  )(
      checkGovernanceLockView: SpliceTestConsoleEnvironment => (View, PartyId) => Assertion,
      checkVestingLockView: SpliceTestConsoleEnvironment => (
          View,
          PartyId,
          BigDecimal,
      ) => Assertion,
  ) =
    s"GovernanceLock TSv${ops.version} compatibility" in { implicit env =>
      // Setup alice as the lock owner
      val ownerParty = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      val owner = RichPartyId.local(ownerParty)

      aliceWalletClient.tap(lockAmount)

      val governanceLockCid = ops.createGovernanceLock(
        aliceValidatorBackend.participantClientWithAdminToken,
        owner,
        lockParty,
        lockSubject = lockSubject,
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
        checkLockKind(governanceLock.specification.kind)
      }

      clue("the GovernanceLock is as a pending TransferInstruction to the magic party") {
        val (cid, view) = ops
          .list(
            aliceValidatorBackend.participantClientWithAdminToken,
            ownerParty,
          )
          .loneElement
        cid.contractId shouldBe governanceLockCid.contractId
        checkGovernanceLockView(env)(view, ownerParty)
      }

      clue("Scan serves a withdraw choice context for the GovernanceLock") {
        ops
          .getWithdrawContext(governanceLockCid)
          .disclosedContracts should not be empty
      }

      // Advance sim-time past the lock's requestedAt time
      advanceTimeAndWaitForRoundOpening

      // Withdraw the governance lock; it's relocked as a VestingLock and funds remain locked
      val unlockAt = getLedgerTime.plusSeconds(1)

      val (vestingLockCid, vestingLockView) = ops
        .unlockGovernanceLock(
          aliceValidatorBackend.participantClientWithAdminToken,
          owner,
          governanceLockCid,
          Map(governanceLockUnlockAtMetaKey -> unlockAt.toMicros.toString),
        )
        .value
      val vestingLock =
        aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
          .filterJava(governancelock.VestingLock.COMPANION)(
            ownerParty,
            predicate = _.id.contractId == vestingLockCid.contractId,
          )
          .loneElement
          .data
      vestingLockCid.contractId should not be governanceLockCid.contractId
      checkVestingLockView(env)(vestingLockView, ownerParty, lockAmount)
      checkLockKind(vestingLock.specification.kind)
      vestingLock.endTime
        .minus(vestingLock.vestingPeriod.microseconds, ChronoUnit.MICROS) shouldBe
        unlockAt.toInstant

      clue("Scan serves a withdraw choice context for the VestingLock") {
        ops
          .getWithdrawContext(vestingLockCid)
          .disclosedContracts should not be empty
      }

      clue("the amulet remains locked while vesting") {
        aliceWalletClient.balance().lockedQty shouldBe lockAmount
      }

      // Advance part of the way into the 2 minute vesting period and withdraw with an explicit
      // withdraw time of 30 seconds into the vesting period
      advanceTime(Duration.ofSeconds(31))

      val (remainingVestingLockCid, remainingVestingLockView) = ops
        .withdrawVestingLock(
          aliceValidatorBackend.participantClientWithAdminToken,
          owner,
          vestingLockCid,
          Map(vestingLockWithdrawAtMetaKey -> unlockAt.plusSeconds(30).toMicros.toString),
        )
        .value
      val remainingVestingLock =
        aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
          .filterJava(governancelock.VestingLock.COMPANION)(
            ownerParty,
            predicate = _.id.contractId == remainingVestingLockCid.contractId,
          )
          .loneElement
          .data
      remainingVestingLockCid.contractId should not be vestingLockCid.contractId
      checkVestingLockView(env)(remainingVestingLockView, ownerParty, expectedRemainingLockAmount)
      checkLockKind(remainingVestingLock.specification.kind)
      val remainingVestingAmount = BigDecimal(remainingVestingLock.vestingAmount)
      remainingVestingAmount shouldBe expectedRemainingLockAmount
      aliceWalletClient.balance().lockedQty shouldBe expectedRemainingLockAmount

      // Advance past the end of the vesting period and withdraw with an explicit withdraw time of
      // the exact end of the vesting period
      advanceTime(Duration.ofSeconds(90))

      ops.withdrawVestingLock(
        aliceValidatorBackend.participantClientWithAdminToken,
        owner,
        remainingVestingLockCid,
        Map(vestingLockWithdrawAtMetaKey -> unlockAt.plusSeconds(120).toMicros.toString),
      ) shouldBe None
      aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
        .filterJava(governancelock.VestingLock.COMPANION)(ownerParty, _ => true) shouldBe empty

      clue("the only LockedAmulet of the owner is the expired LockedAmulet of the vesting lock") {
        val now = getLedgerTime.toInstant
        val remainingLock =
          aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
            .filterJava(amuletCodegen.LockedAmulet.COMPANION)(ownerParty, _ => true)
            .loneElement
            .data
        remainingLock.lock.expiresAt.isAfter(now) shouldBe false
        BigDecimal(remainingLock.amulet.amount.initialAmount) shouldBe remainingVestingAmount
      }

      checkTxHistory(
        aliceWalletClient,
        Seq(
          // Full VestingLock withdraw; it was already fully relocked, so nothing is returned
          { case logEntry: BalanceChangeTxLogEntry =>
            logEntry.transferInstructionCid shouldBe remainingVestingLockCid.contractId
            logEntry.amount shouldBe 0
          },
          // Partial VestingLock withdraw; only the vested portion is returned
          { case logEntry: BalanceChangeTxLogEntry =>
            logEntry.transferInstructionCid shouldBe vestingLockCid.contractId
            logEntry.amount shouldBe lockAmount - expectedRemainingLockAmount
          },
          // GovernanceLock withdraw; the full amount is relocked, nothing is returned
          { case logEntry: BalanceChangeTxLogEntry =>
            logEntry.transferInstructionCid shouldBe governanceLockCid.contractId
            logEntry.amount shouldBe 0
          },
        ),
        ignore = {
          case b: BalanceChangeTxLogEntry =>
            !b.subtype.contains(
              TxLogEntry.BalanceChangeTransactionSubtype.TransferInstruction_Withdraw.toProto
            )
          case _ => true
        },
      )
    }

  private def checkTransferInstructionView(
      view: transferinstructionv1.TransferInstructionView,
      ownerParty: PartyId,
      expectedAmount: BigDecimal,
  ): Assertion = {
    view.transfer.sender shouldBe ownerParty.toProtoPrimitive
    view.transfer.receiver shouldBe lockParty.toProtoPrimitive
    BigDecimal(view.transfer.amount) shouldBe expectedAmount
    view.transfer.meta.values
      .get(TokenStandardMetadata.reasonMetaKey) shouldBe makeLockSubject(lockSubject)
  }

  testTokenStandardCompat(tsv1Operations)(
    _ => checkTransferInstructionView(_, _, lockAmount),
    _ => checkTransferInstructionView,
  )

  private def checkAllocationView(
      view: allocationv2.AllocationView,
      ownerParty: PartyId,
      expectedAmount: BigDecimal,
  )(implicit env: SpliceTestConsoleEnvironment): Assertion = {
    view.settlement.id shouldBe lockTypeForMagicParty(lockParty)
    view.settlement.executors.asScala shouldBe Seq(dsoParty.toProtoPrimitive)
    view.allocation.authorizer.owner.toScala shouldBe Some(ownerParty.toProtoPrimitive)
    view.allocation.transferLegSides.asScala shouldBe empty
    view.allocation.committed shouldBe false
    view.allocation.meta.values
      .get(TokenStandardMetadata.reasonMetaKey) shouldBe makeLockSubject(lockSubject)
    view.holdingCids.asScala should have size 1
    view.availableActions.asScala.view
      .mapValues(_.asScala.map(_.asScala.toSeq).toSeq)
      .toMap shouldBe
      Map(
        new allocationv2.allocationaction.AA_Withdraw(javaapi.data.Unit.getInstance()) ->
          Seq(Seq(ownerParty.toProtoPrimitive))
      )
    val nextIterationFunding = view.allocation.nextIterationFunding.toScala.value.asScala.toMap
    nextIterationFunding.size shouldBe 1
    nextIterationFunding.keys.head shouldBe "Amulet"
    BigDecimal(nextIterationFunding.values.head) shouldBe expectedAmount
  }

  testTokenStandardCompat(tsv2Operations)(
    implicit env => checkAllocationView(_, _, lockAmount),
    implicit env => checkAllocationView,
  )
}
