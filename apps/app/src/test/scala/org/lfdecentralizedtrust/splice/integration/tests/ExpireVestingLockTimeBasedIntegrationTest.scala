// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.HasExecutionContext
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.da.time.types.RelTime
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.{Amulet, LockedAmulet}
import org.lfdecentralizedtrust.splice.codegen.java.splice.fees.{ExpiringAmount, RatePerRound}
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock.{
  VestingLock,
  VestingLockSpecification,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock.governancelockkind.GLK_SuperValidatorRightsOwner
import org.lfdecentralizedtrust.splice.codegen.java.splice.expiry.TimeLock
import org.lfdecentralizedtrust.splice.codegen.java.splice.types.Round
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.config.ConfigTransforms.{
  ConfigurableApp,
  updateAutomationConfig,
}
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTestWithIsolatedEnvironment,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.{
  ExpireVestingLockTrigger,
  ExpiredAmuletTrigger,
  ExpiredLockedAmuletTrigger,
}
import org.lfdecentralizedtrust.splice.util.{TimeTestUtil, TriggerTestUtil, WalletTestUtil}

import java.time.{Duration, Instant}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Covers `ExpireVestingLockTrigger`: batched archival of fully vested `VestingLock`s.
  *
  * The trigger is a [[org.lfdecentralizedtrust.splice.automation.BatchedMultiDomainExpiredContractTrigger]],
  * and `runOnce()` on a polling-parallel trigger processes exactly the head task, i.e. one batch.
  * With `delegatelessAutomationExpiredVestingLockBatchSize = 2` and three expired locks that makes
  * the batching observable and deterministic: 2, then 1, then nothing left to do.
  */
@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceAmulet_0_1_24
@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_30
class ExpireVestingLockTimeBasedIntegrationTest
    extends IntegrationTestWithIsolatedEnvironment
    with HasExecutionContext
    with WalletTestUtil
    with TimeTestUtil
    with TriggerTestUtil {

  // We create `Amulet`/`LockedAmulet`/`VestingLock` directly as root contracts, which the
  // token-standard and update-history parsers do not expect.
  override protected def runTokenStandardCliSanityCheck: Boolean = false
  override protected def runUpdateHistorySanityCheck: Boolean = false

  private val batchSize = 2
  private val numLocks = 3
  private val vestingDuration = Duration.ofHours(1)

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)
      .withoutAutomaticRewardsCollectionAndAmuletMerging
      // The trigger under test is driven explicitly via `runOnce()`, and we assert that the
      // `LockedAmulet` survives `VestingLock` expiry, so the amulet expiry triggers stay paused.
      .addConfigTransforms((_, config) =>
        updateAutomationConfig(ConfigurableApp.Sv)(
          _.withPausedTrigger[ExpireVestingLockTrigger]
            .withPausedTrigger[ExpiredLockedAmuletTrigger]
            .withPausedTrigger[ExpiredAmuletTrigger]
        )(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppConfigs_(
          _.copy(delegatelessAutomationExpiredVestingLockBatchSize = batchSize)
        )(config)
      )

  "ExpireVestingLockTrigger archives fully vested VestingLocks in batches" in { implicit env =>
    val sv1UserId = sv1Backend.config.ledgerApiUser
    val sv1Party = sv1Backend.getDsoInfo().svParty

    val endTime = getLedgerTime.toInstant.plus(vestingDuration)

    val (lockedAmuletCids, _) = actAndCheck(
      s"Create $numLocks LockedAmulet/VestingLock pairs owned by sv1, vesting until $endTime", {
        (1 to numLocks).map { _ =>
          val lockedAmuletCid = createLockedAmuletForVesting(sv1UserId, sv1Party, endTime)
          createVestingLock(sv1UserId, sv1Party, lockedAmuletCid, endTime)
          lockedAmuletCid
        }
      },
    )(
      "SvDsoStore ingests all VestingLocks",
      _ => listVestingLocks should have length numLocks.toLong,
    )

    clue("The locks are not yet expired, so the trigger has no work") {
      expireVestingLockTrigger.runOnce().futureValue shouldBe false
      listVestingLocks should have length numLocks.toLong
    }

    // `listExpiredFromPayloadExpiry` uses a strict `expires_at < now`, so we step strictly past
    // `endTime` rather than exactly onto it.
    advanceTime(vestingDuration.plus(Duration.ofMinutes(1)))

    clue("The locks stay put while the trigger is paused") {
      listVestingLocks should have length numLocks.toLong
    }

    clue(s"First run archives one full batch of $batchSize") {
      expireVestingLockTrigger.runOnce().futureValue shouldBe true
      listVestingLocks should have length (numLocks - batchSize).toLong
    }

    clue("Second run archives the remaining partial batch") {
      expireVestingLockTrigger.runOnce().futureValue shouldBe true
      listVestingLocks shouldBe empty
    }

    clue("Third run is a no-op: the trigger is idempotent once everything is expired") {
      expireVestingLockTrigger.runOnce().futureValue shouldBe false
      listVestingLocks shouldBe empty
    }

    clue(
      "The LockedAmulets survive: VestingLock_DsoExpire archives only the wrapper, and " +
        "ExpiredLockedAmuletTrigger collects the amulet independently"
    ) {
      val remaining = sv1Backend.appState.dsoStore.multiDomainAcsStore
        .listContracts(LockedAmulet.COMPANION)
        .futureValue
        .map(_.contractId.contractId)
        .toSet
      lockedAmuletCids.map(_.contractId).toSet.subsetOf(remaining) shouldBe true
    }
  }

  private def sv1Name(implicit env: SpliceTestConsoleEnvironment): String = {
    val info = sv1Backend.getDsoInfo()
    info.dsoRules.payload.svs.get(info.svParty.toProtoPrimitive).name
  }

  private def expireVestingLockTrigger(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.dsoDelegateBasedAutomation.trigger[ExpireVestingLockTrigger]

  private def listVestingLocks(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.appState.dsoStore.multiDomainAcsStore
      .listContracts(VestingLock.COMPANION)
      .futureValue

  /** The `LockedAmulet` a `VestingLock` wraps is relocked by `GovernanceLock_Unlock` with
    * `expiresAt = endTime`, so we mirror that here rather than using the generic
    * `WalletTestUtil.createLockedAmulet` helper, which takes a relative duration.
    */
  private def createLockedAmuletForVesting(
      userId: String,
      owner: PartyId,
      endTime: Instant,
  )(implicit env: SpliceTestConsoleEnvironment): LockedAmulet.ContractId = {
    val amulet = new Amulet(
      dsoParty.toProtoPrimitive,
      owner.toProtoPrimitive,
      new ExpiringAmount(
        BigDecimal(100.0).bigDecimal,
        new Round(0L),
        // Must be non-zero: `SvDsoStore`'s `LockedAmulet` filter derives the round of expiry by
        // dividing the amount by this rate, and a zero rate throws out of the ingestion loop.
        new RatePerRound(BigDecimal(0.01).bigDecimal),
      ),
    )
    val lockedAmulet = new LockedAmulet(
      amulet,
      new TimeLock(
        Seq(dsoParty.toProtoPrimitive).asJava,
        endTime,
        None.toJava,
      ),
    )
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.commands
      .submitWithResult(
        userId = userId,
        actAs = Seq(dsoParty, owner),
        readAs = Seq.empty,
        update = lockedAmulet.create(),
      )
      .contractId
  }

  private def createVestingLock(
      userId: String,
      owner: PartyId,
      lockedAmulet: LockedAmulet.ContractId,
      endTime: Instant,
  )(implicit env: SpliceTestConsoleEnvironment): VestingLock.ContractId = {
    val vestingLock = new VestingLock(
      dsoParty.toProtoPrimitive,
      owner.toProtoPrimitive,
      lockedAmulet,
      new RelTime(vestingDuration.toMillis * 1000L),
      endTime,
      BigDecimal(100.0).bigDecimal,
      new VestingLockSpecification(
        new GLK_SuperValidatorRightsOwner(sv1Name)
      ),
    )
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.commands
      .submitWithResult(
        userId = userId,
        actAs = Seq(dsoParty, owner),
        readAs = Seq.empty,
        update = vestingLock.create(),
      )
      .contractId
  }
}
