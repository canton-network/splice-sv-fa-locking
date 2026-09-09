// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.IntegrationTestWithIsolatedEnvironment
import org.lfdecentralizedtrust.splice.util.{TimeTestUtil, WalletTestUtil}

import scala.reflect.ClassTag

class GovernanceLockTimeBasedIntegrationTest
    extends IntegrationTestWithIsolatedEnvironment
    with WalletTestUtil
    with ExternallySignedPartyTestUtil
    with TimeTestUtil
    with TokenStandardTest {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)

  sealed abstract class GovernanceLockKind[K <: governancelock.GovernanceLockKind: ClassTag] {
    final val matchKind: PartialFunction[governancelock.GovernanceLockKind, Unit] = { case _: K => }
  }
  object GovernanceLockKind {
    case object FA extends GovernanceLockKind[governancelock.governancelockkind.GLK_FeaturedApp]
    case object SV
        extends GovernanceLockKind[governancelock.governancelockkind.GLK_SuperValidatorRightsOwner]
  }

  private def testTSv1Compatibility(kind: GovernanceLockKind[?]) = {
    s"$kind GovernanceLock created via TSv1 compatibility interface can be unlocked into a VestingLock" in {
      implicit env =>
        val lockAmount = BigDecimal(10000.0)

        // Alice is the lock owner
        aliceValidatorWalletClient.tap(lockAmount)
        val owner = onboardAndSetupExternalParty(aliceValidatorBackend, Some("lockOwner"))
        actAndCheck(
          "Fund the external lock owner",
          aliceValidatorWalletClient.transferPreapprovalSend(owner.party, lockAmount, ""),
        )(
          "the owner sees enough unlocked funds to create the lock",
          _ =>
            aliceValidatorBackend
              .getExternalPartyBalance(owner.party)
              .totalUnlockedCoin shouldBe lockAmount.setScale(10).toString,
        )

        val (magicParty, lockSubject) = kind match {
          case GovernanceLockKind.FA =>
            grantFeaturedAppRight(aliceValidatorWalletClient)
            (featuredAppLockMagicParty, aliceValidatorWalletClient.userStatus().party)
          case GovernanceLockKind.SV =>
            (superValidatorLockMagicParty, "sv1")
        }

        // Create the governance lock via a transfer to the magic party
        val governanceLockCid = createGovernanceLockViaTokenStandard(
          aliceValidatorBackend.participantClientWithAdminToken,
          owner.richPartyId,
          magicParty,
          lockSubject = lockSubject,
          amount = lockAmount,
        )

        clue("the GovernanceLock has the correct kind") {
          val governanceLock =
            aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
              .filterJava(governancelock.GovernanceLock.COMPANION)(
                owner.party,
                predicate = _.id.contractId == governanceLockCid.contractId,
              )
              .loneElement
              .data
          kind.matchKind.isDefinedAt(governanceLock.specification.kind) shouldBe true
        }

        clue("the GovernanceLock is as a pending TransferInstruction to the magic party") {
          val (cid, view) = listTransferInstructions(
            aliceValidatorBackend.participantClientWithAdminToken,
            owner.party,
          ).loneElement
          cid.contractId shouldBe governanceLockCid.contractId
          view.transfer.sender shouldBe owner.party.toProtoPrimitive
          view.transfer.receiver shouldBe magicParty.toProtoPrimitive
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
            owner.richPartyId,
            governanceLockCid,
          ),
        )(
          "a VestingLock is now the pending TransferInstruction to the magic party",
          _ => {
            val (cid, view) = listTransferInstructions(
              aliceValidatorBackend.participantClientWithAdminToken,
              owner.party,
            ).loneElement
            val vestingLock =
              aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
                .filterJava(governancelock.VestingLock.COMPANION)(
                  owner.party,
                  predicate = _.id.contractId == cid.contractId,
                )
                .loneElement
                .data
            cid.contractId should not be governanceLockCid.contractId
            view.transfer.sender shouldBe owner.party.toProtoPrimitive
            view.transfer.receiver shouldBe magicParty.toProtoPrimitive
            kind.matchKind.isDefinedAt(vestingLock.specification.kind) shouldBe true
            cid
          },
        )

        clue("Scan serves a withdraw choice context for the VestingLock") {
          sv1ScanBackend
            .getTransferInstructionWithdrawContext(vestingLockCid)
            .disclosedContracts should not be empty
        }

        clue("the amulet remains locked while vesting") {
          aliceValidatorBackend
            .getExternalPartyBalance(owner.party)
            .totalLockedCoin shouldBe lockAmount.setScale(10).toString
        }
    }
  }

  testTSv1Compatibility(GovernanceLockKind.FA)
  testTSv1Compatibility(GovernanceLockKind.SV)
}
