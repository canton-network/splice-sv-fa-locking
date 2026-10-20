// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.HasExecutionContext
import com.digitalasset.canton.config.NonNegativeFiniteDuration
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock.GovernanceLock
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock.transfer.GovernanceLockTransfer
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.config.ConfigTransforms.{
  ConfigurableApp,
  updateAutomationConfig,
}
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTestWithIsolatedEnvironment,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.{
  ExpireGovernanceLockTransferTrigger,
  ExpireVestingLockTrigger,
}
import org.lfdecentralizedtrust.splice.sv.config.InitialGovernanceLockConfig
import org.lfdecentralizedtrust.splice.util.{
  GovernanceLockUtil,
  TimeTestUtil,
  TriggerTestUtil,
  WalletTestUtil,
}

import java.time.Duration

/** Covers `ExpireGovernanceLockTransferTrigger`: batched archival of expired
  * `GovernanceLockTransfer` proposals.
  *
  * A proposal to transfer a vesting lock expires at the `endTime` of that lock (or earlier, at
  * `requestedAt + tokenStandardMaxTTL`). We use a vesting lock with a test-sized vesting period
  * so the proposals expire inside one round tick. A full lock as the source would need a sim-time
  * advance of `tokenStandardMaxTTL`, which defaults to 90 days.
  *
  * The trigger is a
  * [[org.lfdecentralizedtrust.splice.automation.BatchedMultiDomainExpiredContractTrigger]],
  * and `runOnce()` on a polling-parallel trigger processes exactly the head task. One task in one
  * batch in case of `ExpiredLockedAmuletTrigger`.
  * With `delegatelessAutomationExpiredVestingLockBatchSize = 2` and three expired locks that makes
  * the batching observable and deterministic: 2, then 1, then nothing left to do.
  */
@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceAmulet_0_1_25
class ExpireGovernanceLockTransferTimeBasedIntegrationTest
    extends IntegrationTestWithIsolatedEnvironment
    with HasExecutionContext
    with WalletTestUtil
    with TimeTestUtil
    with TriggerTestUtil
    with GovernanceLockTest {

  private val batchSize = 2
  private val numTransfers = 3

  // Sim-time budget, all of it inside one 10-minute round tick
  // (`SpliceUtil.defaultInitialTickDuration`):
  //   t0            fixture built, lock transfers proposed
  //   t0 + 1 min    vestingStartTime
  //   t0 + 6 min    endTime  (= vestingStartTime + vestingDuration) = expiresAt of the proposals
  //   t0 + 7 min    after advanceTime (strictly past expiresAt)
  // Advances spanning many round ticks make the round automation work through a backlog that
  // everything afterwards then races; see canton-network/splice#7223.
  private val unlockDelay = Duration.ofMinutes(1)
  private val vestingDuration = Duration.ofMinutes(5)
  private val totalAdvance = Duration.ofMinutes(7)

  private val lockKind = SuperValidatorLock
  private val lockAmount = minimumLockAmount
  // A proposal moves no funds, so the proposals can together exceed the lock amount
  private val transferAmount = BigDecimal(1000.0)

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)
      .withoutAutomaticRewardsCollectionAndAmuletMerging
      .addConfigTransforms((_, config) =>
        updateAutomationConfig(ConfigurableApp.Sv)(
          _.withPausedTrigger[ExpireGovernanceLockTransferTrigger]
            // Keeps the source vesting lock in place once it is fully vested
            .withPausedTrigger[ExpireVestingLockTrigger]
        )(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppConfigs_(
          _.copy(delegatelessAutomationExpiredGovernanceLockTransferBatchSize = batchSize)
        )(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppFoundDsoConfigs_(
          _.copy(
            initialGovernanceLockConfig = Some(
              InitialGovernanceLockConfig(
                superValidatorLockVestingDuration =
                  Some(NonNegativeFiniteDuration.ofMillis(vestingDuration.toMillis))
              )
            )
          )
        )(config)
      )

  "ExpireGovernanceLockTransferTrigger archives expired GovernanceLockTransfers in batches" in {
    implicit env =>
      val sv1Party = sv1Backend.getDsoInfo().svParty
      val owner = RichPartyId.local(sv1Party)
      val participant = sv1Backend.participantClientWithAdminToken
      val newLockOwner = aliceValidatorBackend.getValidatorPartyId()

      val vestingStartTime = getLedgerTime.plus(unlockDelay)

      // `tsv1Operations.createGovernanceLock` feeds all of the owner's unlocked
      // holdings into the `TransferFactory_Transfer` choice. Thus, the tap has to
      // have landed before the lock is submitted.
      actAndCheck(
        s"Tap $lockAmount CC for sv1",
        sv1WalletClient.tap(walletAmuletToUsd(lockAmount)),
      )(
        "the tapped amulet is visible",
        _ => listHoldings(participant, sv1Party).filter(_._2.lock.isEmpty) should have length 1,
      )

      val (_, vestingLock) = actAndCheck(
        "Lock and unlock, so that the lock starts vesting",
        tsv1Operations.createAndUnlockGovernanceLock(
          participant,
          owner,
          lockKind,
          lockSubject = sv1Name,
          lockAmount = lockAmount,
          unlockAmount = lockAmount,
          vestingStartTime = vestingStartTime,
        ),
      )(
        "SvDsoStore ingests the VestingLock",
        _ => listVestingLocks.loneElement.payload,
      )

      actAndCheck(
        s"Propose $numTransfers transfers of the VestingLock to $newLockOwner",
        (1 to numTransfers).foreach { _ =>
          tsv1Operations.transferVestingLock(vestingLock)(
            participant,
            owner,
            lockKind,
            sv1Name,
            newLockOwner,
            transferAmount,
          )
        },
      )(
        "SvDsoStore ingests all GovernanceLockTransfers",
        _ => listLockTransfers should have length numTransfers.toLong,
      )

      clue("The lock transfers are not yet expired, so the trigger has no work") {
        expireLockTransferTrigger.runOnce().futureValue shouldBe false
        listLockTransfers should have length numTransfers.toLong
      }

      // `listExpiredGovernanceLockTransfers` uses a strict `contract_expires_at < now`, so we
      // step strictly past `expiresAt` rather than exactly onto it.
      advanceTime(totalAdvance)

      clue("The lock transfers stay put while the trigger is paused") {
        listLockTransfers should have length numTransfers.toLong
      }

      clue(s"First run archives one full batch of $batchSize") {
        expireLockTransferTrigger.runOnce().futureValue shouldBe true
        listLockTransfers should have length (numTransfers - batchSize).toLong
      }

      clue("Second run archives the remaining partial batch") {
        expireLockTransferTrigger.runOnce().futureValue shouldBe true
        listLockTransfers shouldBe empty
      }

      clue("The source VestingLock is left untouched") {
        listVestingLocks.loneElement.payload shouldBe vestingLock
      }
  }

  private def sv1Name(implicit env: SpliceTestConsoleEnvironment): String = {
    val info = sv1Backend.getDsoInfo()
    info.dsoRules.payload.svs.get(info.svParty.toProtoPrimitive).name
  }

  private def expireLockTransferTrigger(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.dsoDelegateBasedAutomation.trigger[ExpireGovernanceLockTransferTrigger]

  private def listLockTransfers(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.appState.dsoStore.multiDomainAcsStore
      .listContracts(GovernanceLockTransfer.COMPANION)
      .futureValue

  private def listVestingLocks(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.appState.dsoStore.multiDomainAcsStore
      .listContracts(GovernanceLock.COMPANION)
      .futureValue
      .filter(lock => GovernanceLockUtil.getVestingLockSpecification(lock.payload).isDefined)
}
