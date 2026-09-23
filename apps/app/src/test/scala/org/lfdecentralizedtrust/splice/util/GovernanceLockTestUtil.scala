// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.util

import com.digitalasset.canton.HasExecutionContext
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.Amulet
import org.lfdecentralizedtrust.splice.codegen.java.splice.api.token.holdingv1
import org.lfdecentralizedtrust.splice.codegen.java.splice.api.token.metadatav1
import org.lfdecentralizedtrust.splice.codegen.java.splice.api.token.metadatav1.AnyContract
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock.{
  GovernanceLock,
  GovernanceLockSpecification,
  VestingLock,
}
import org.lfdecentralizedtrust.splice.console.ParticipantClientReference
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  SpliceTestConsoleEnvironment,
  TestCommon,
}

import java.time.Instant
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Scala mirrors of the `Splice.Scripts.TestGovernanceLocks` helpers, so that integration tests can
  * build `GovernanceLock`s and `VestingLock`s through the production choices instead of
  * hand-creating them.
  *
  * The correspondence is deliberate and greppable:
  *
  *   - [[lockForGovernance]] mirrors `governanceLockRawChoice`
  *     (`ExternalPartyAmuletRules_LockForGovernance`),
  *   - [[unlockGovernanceLock]] mirrors `governanceUnlockRawChoice` (`GovernanceLock_Unlock`),
  *   - [[getGovernanceLockContext]] mirrors `submitWithTransferContext` /
  *     `AmuletRegistryV2.getExternalPartyConfigStateContext`.
  */
trait GovernanceLockTestUtil extends TestCommon { this: HasExecutionContext =>

  /** The one-entry choice context both governance-lock choices expect.
    *
    * `unfeaturedPaymentContextAndConfigFromChoiceContext` fails the transaction unless the context
    * carries the `external-party-config-state` key, and `ExternalPartyConfigState` has no contract
    * key and two active contracts at a time, so the submitter has to pick one. Neither choice
    * checks freshness, so the newest of the pair is always fine.
    *
    * No disclosures are needed as long as the submission runs on the participant hosting the DSO
    * party with `readAs = dso`: `ExternalPartyAmuletRules` and `ExternalPartyConfigState` are both
    * `signatory dso`.
    */
  def getGovernanceLockContext(implicit
      env: SpliceTestConsoleEnvironment
  ): ChoiceContextWithDisclosures = {
    val configState = sv1Backend.appState.dsoStore
      .lookupLatestExternalPartyConfigState()
      .futureValue
      .value
    ChoiceContextWithDisclosures(
      disclosedContracts = Seq.empty,
      choiceContext = new metadatav1.ChoiceContext(
        Map[String, metadatav1.AnyValue](
          "external-party-config-state" -> new metadatav1.anyvalue.AV_ContractId(
            new AnyContract.ContractId(configState.contractId.contractId)
          )
        ).asJava
      ),
    )
  }

  /** The unlocked `Amulet`s of `owner`, as the `inputs` the token-standard choices expect.
    *
    * Mirrors `WalletClientV2.listUnlockedHoldingCidsFor`: `LockedAmulet`s are a separate template,
    * so filtering on `Amulet` already yields only unlocked holdings.
    */
  def listUnlockedHoldingCids(
      participantClient: ParticipantClientReference,
      owner: PartyId,
  ): Seq[holdingv1.Holding.ContractId] =
    participantClient.ledger_api_extensions.acs
      .filterJava(Amulet.COMPANION)(owner)
      .map(amulet => new holdingv1.Holding.ContractId(amulet.id.contractId))

  /** Scala mirror of `TestGovernanceLocks.governanceLockRawChoice`.
    *
    * Locks `amount` out of all of `owner`'s unlocked holdings. The remainder
    * comes back as a change `Amulet`.
    */
  def lockForGovernance(
      participantClient: ParticipantClientReference,
      userId: String,
      owner: PartyId,
      amount: BigDecimal,
      specification: GovernanceLockSpecification,
  )(implicit env: SpliceTestConsoleEnvironment): GovernanceLock.ContractId = {
    val rules = sv1ScanBackend.getExternalPartyAmuletRules()
    val inputs = listUnlockedHoldingCids(participantClient, owner)
    participantClient.ledger_api_extensions.commands
      .submitWithResult(
        userId = userId,
        actAs = Seq(owner),
        readAs = Seq(dsoParty),
        update = rules.contractId.exerciseExternalPartyAmuletRules_LockForGovernance(
          amount.bigDecimal,
          inputs.asJava,
          specification,
          getGovernanceLockContext.toExtraArgs(),
          owner.toProtoPrimitive,
          participantClient.ledger_api.time.get().toInstant,
          new metadatav1.Metadata(java.util.Map.of()),
        ),
      )
      .exerciseResult
      .governanceLock
  }

  /** Scala mirror of `TestGovernanceLocks.governanceUnlockRawChoice`.
    *
    * `unlockAmount = None` unlocks the whole lock (no split, no continuing `GovernanceLock`).
    * `unlockAt` must be strictly in the future; the resulting `VestingLock.endTime` is
    * `unlockAt + vestingDurationFor specification.kind`, read off the `ExternalPartyConfigState`.
    * No `VestingLock` is created if that vesting duration is zero.
    */
  def unlockGovernanceLock(
      participantClient: ParticipantClientReference,
      userId: String,
      lock: GovernanceLock.ContractId,
      unlockAmount: Option[BigDecimal],
      unlockAt: Instant,
      actors: Seq[PartyId],
  )(implicit
      env: SpliceTestConsoleEnvironment
  ): (Option[VestingLock.ContractId], Option[GovernanceLock.ContractId]) = {
    val result = participantClient.ledger_api_extensions.commands
      .submitWithResult(
        userId = userId,
        actAs = actors,
        readAs = Seq(dsoParty),
        update = lock.exerciseGovernanceLock_Unlock(
          unlockAmount.map(_.bigDecimal).toJava,
          unlockAt,
          getGovernanceLockContext.toExtraArgs(),
          actors.map(_.toProtoPrimitive).asJava,
        ),
      )
      .exerciseResult
    (result.vestingLock.toScala, result.governanceLock.toScala)
  }
}
