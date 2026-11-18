// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.logging.{LogEntry, SuppressionRule}
import com.digitalasset.canton.discard.Implicits.DiscardOps
import com.digitalasset.canton.topology.{ForceFlag, PartyId}
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.console.{
  ParticipantClientReference,
  WalletAppClientReference,
}
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTestWithIsolatedEnvironment,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.ExpireVestingLockTrigger
import org.lfdecentralizedtrust.splice.sv.config.InitialGovernanceLockConfig
import org.lfdecentralizedtrust.splice.util.{GovernanceLockUtil, TimeTestUtil, WalletTestUtil}
import org.slf4j.event.Level

import java.time.{Duration, Instant}
import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.OptionConverters.*

/** Demonstrates that one vesting lock that can't be expired stalls
  * `ExpireVestingLockTrigger` for other locks.
  *
  * The assertions state the desired behaviour, so the test fails where the
  * trigger stalls.
  */
@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceAmulet_0_1_25
class ExpireVestingLockStallTimeBasedIntegrationTest
    extends IntegrationTestWithIsolatedEnvironment
    with WalletTestUtil
    with TimeTestUtil
    with GovernanceLockTest {

  private type LockCid = governancelock.GovernanceLock.ContractId

  private val triggerBatchSize = 2
  private val vestingDuration = Duration.ofMinutes(2)
  // The gap between the vesting starts of the early sv-lock, the fa-lock and
  // the late sv-lock, in that order.
  private val vestingStartGap = Duration.ofMinutes(1)
  // Shrinks the retry backoff of `SvTaskBasedTrigger` (5-10 s by default) so
  // that a task that never succeeds runs out of retries within seconds.
  private val expectedTaskDurationMillis = 100L

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppFoundDsoConfigs_(
          _.copy(
            initialGovernanceLockConfig = Some(
              InitialGovernanceLockConfig(
                superValidatorLockVestingDuration =
                  Some(NonNegativeFiniteDuration.tryFromJavaDuration(vestingDuration)),
                featuredAppLockVestingDuration =
                  Some(NonNegativeFiniteDuration.tryFromJavaDuration(vestingDuration)),
              )
            )
          )
        )(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppConfigs_(
          _.copy(
            delegatelessAutomationExpiredVestingLockBatchSize = triggerBatchSize,
            delegatelessAutomationExpectedTaskDuration = expectedTaskDurationMillis,
          )
        )(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAutomationConfig(ConfigTransforms.ConfigurableApp.Sv)(
          _.withPausedTrigger[ExpireVestingLockTrigger]
        )(config)
      )

  "a vesting lock whose provider is no longer hosted stops expiry of its owner's later locks" in {
    implicit env =>
      val sv1Party = sv1Backend.getDsoInfo().svParty
      val owner = RichPartyId.local(sv1Party)
      val participant = sv1Backend.participantClientWithAdminToken
      val participantId = participant.id

      val provider = clue("Allocate a provider party hosted only on sv1") {
        participant.ledger_api.parties
          .allocate(
            // Random, as the Canton instance outlives this test case.
            s"stall-provider-${UUID.randomUUID().toString.take(8)}",
            synchronizerId = Some(decentralizedSynchronizerId),
          )
          .party
      }

      val earlyStartTime = getLedgerTime.plusSeconds(1).toInstant
      val poisonStartTime = earlyStartTime.plus(vestingStartGap)
      val lateStartTime = poisonStartTime.plus(vestingStartGap)

      tapForLocks(sv1WalletClient, participant, owner, numLocks = 3)

      val earlyCid = vestingLock(participant, owner, SuperValidatorLock, "sv1", earlyStartTime)(
        s"Lock and unlock an sv-lock, vesting from $earlyStartTime"
      )
      val poisonCid = vestingLock(
        participant,
        owner,
        FeaturedAppLock,
        provider.toProtoPrimitive,
        poisonStartTime,
      )(
        s"Lock and unlock an fa-lock for $provider, " +
          s"vesting from $poisonStartTime"
      )
      val lateCid = vestingLock(participant, owner, SuperValidatorLock, "sv1", lateStartTime)(
        s"Lock and unlock an sv-lock, vesting from $lateStartTime"
      )
      val labels = Map(poisonCid -> "poison", earlyCid -> "early", lateCid -> "late")

      actAndCheck(
        s"Remove the only hosting of $provider, an observer of the fa-lock",
        sv1Backend.appState.participantAdminConnection
          .ensurePartyToParticipantRemoved(
            decentralizedSynchronizerId,
            provider,
            participantId,
            ForceFlag.DisablePartyWithActiveContracts,
          )
          .futureValue,
      )(
        s"$provider is not hosted anywhere",
        _ =>
          sv1Backend.appState.participantAdminConnection
            .listPartyToParticipantFromAllStores(
              filterParty = provider.filterString
            )
            .futureValue shouldBe empty,
      )

      // The trigger reads `now` 5 s behind the clock, and the listing uses a
      // strict `expires_at < now`, so step well past each `endTime`.
      advanceTime(vestingDuration.plusSeconds(10))

      clue("The first run expires the early sv-lock and leaves the other two") {
        runAndRecord(1, labels) should contain theSameElementsAs Seq(poisonCid, lateCid)
      }

      advanceTime(vestingStartGap)

      clue("The second run can't expire the fa-lock; the late lock isn't due") {
        runAndRecord(2, labels) should contain theSameElementsAs
          Seq(poisonCid, lateCid)
      }

      advanceTime(vestingStartGap)

      clue("The third run expires the late sv-lock, leaving only the fa-lock") {
        runAndRecord(3, labels) shouldBe Seq(poisonCid)
      }
  }

  private def trigger(implicit env: SpliceTestConsoleEnvironment): ExpireVestingLockTrigger =
    sv1Backend.dsoDelegateBasedAutomation.trigger[ExpireVestingLockTrigger]

  private def tapForLocks(
      wallet: WalletAppClientReference,
      participant: ParticipantClientReference,
      owner: RichPartyId,
      numLocks: Int,
  ): Unit =
    actAndCheck(
      s"Tap ${numLocks * minimumLockAmount} CC for ${owner.partyId}",
      wallet.tap(walletAmuletToUsd(numLocks * minimumLockAmount)),
    )(
      "the tapped amulet is visible",
      _ =>
        listHoldings(participant, owner.partyId)
          .filter(_._2.lock.isEmpty) should have length 1,
    ).discard

  /** Creates a full lock and unlocks all of it into a vesting lock. */
  private def vestingLock(
      participant: ParticipantClientReference,
      owner: RichPartyId,
      kind: LockKind,
      subject: String,
      vestingStartTime: Instant,
  )(clueText: String)(implicit env: SpliceTestConsoleEnvironment): LockCid = {
    val before = listVestingLocks.map(_.contractId).toSet
    actAndCheck(
      clueText, {
        val cid = tsv1Operations.createGovernanceLock(
          participant,
          owner,
          kind,
          lockSubject = subject,
          amount = minimumLockAmount,
        )
        tsv1Operations
          .unlockGovernanceLock(
            participant,
            owner,
            kind,
            lockSubject = subject,
            unlockAmount = minimumLockAmount,
            vestingStartTime = vestingStartTime,
            isValid = _.originalInstructionCid.toScala.exists(_.contractId == cid.contractId),
          )
          .discard
      },
    )(
      "SvDsoStore ingests the new vesting lock",
      _ => listVestingLocks.map(_.contractId).filterNot(before).loneElement,
    )._2
  }

  /** Runs the paused trigger once and logs, as `Stall evidence, run N`, how
    * long it took, which locks remain, the unavailable parties, and every
    * WARN+ log line. Returns the remaining vesting locks.
    */
  private def runAndRecord(
      run: Int,
      labels: Map[LockCid, String],
  )(implicit env: SpliceTestConsoleEnvironment): Seq[LockCid] = {
    val problems = new AtomicReference(Seq.empty[LogEntry])
    val started = System.nanoTime()
    loggerFactory.assertLogsSeq(SuppressionRule.LevelAndAbove(Level.WARN))(
      Await.result(trigger.runOnce(), 5.minutes).discard,
      entries => {
        problems.set(entries)
        succeed
      },
    )
    val tookMillis = (System.nanoTime() - started) / 1000000
    val remaining = listVestingLocks.map(_.contractId)
    logger.info(
      s"Stall evidence, run $run: took $tookMillis ms, " +
        s"remaining=${remaining.map(label(labels))}, " +
        s"ignored parties=$ignoredParties, " +
        s"${problems.get.size} WARN+ entries:\n" +
        problems.get
          .map(e => s"  ${e.level} ${e.loggerName}: ${e.message.take(600)}")
          .mkString("\n")
    )
    remaining
  }

  private def label(labels: Map[LockCid, String])(cid: LockCid): String =
    labels.getOrElse(cid, cid.contractId)

  private def ignoredParties(implicit env: SpliceTestConsoleEnvironment): Seq[PartyId] =
    sv1Backend.dsoDelegateBasedAutomation.unavailablePartiesStore.listParties().futureValue

  private def listVestingLocks(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.appState.dsoStore.multiDomainAcsStore
      .listContracts(governancelock.GovernanceLock.COMPANION)
      .futureValue
      .filter(lock => GovernanceLockUtil.getVestingLockSpecification(lock.payload).isDefined)
}
