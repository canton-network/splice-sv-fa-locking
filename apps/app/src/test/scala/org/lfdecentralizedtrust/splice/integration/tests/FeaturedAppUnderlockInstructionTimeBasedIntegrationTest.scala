// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.HasExecutionContext
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.da.time.types.RelTime
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.{
  DsoRulesConfig,
  DsoRules_CreateFeaturedAppUnderlockInstruction,
  DsoRules_SetConfig,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.actionrequiringconfirmation.ARC_DsoRules
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.dsorules_actionrequiringconfirmation.{
  SRARC_CreateFeaturedAppUnderlockInstruction,
  SRARC_SetConfig,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.round.OpenMiningRound
import org.lfdecentralizedtrust.splice.config.ConfigTransforms.{
  ConfigurableApp,
  updateAutomationConfig,
}
import org.lfdecentralizedtrust.splice.console.{ValidatorAppReference, WalletAppClientReference}
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTestWithIsolatedEnvironment,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.automation.confirmation.FeaturedAppUnderlockStatusUpdateTrigger
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.ExecuteFeaturedAppUnderlockInstructionTrigger
import org.lfdecentralizedtrust.splice.util.{TimeTestUtil, WalletTestUtil}

import java.time.{Duration, Instant}
import java.util.Optional
import scala.jdk.OptionConverters.*

@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_32
class FeaturedAppUnderlockInstructionTimeBasedIntegrationTest
    extends IntegrationTestWithIsolatedEnvironment
    with HasExecutionContext
    with WalletTestUtil
    with TimeTestUtil
    with FeaturedAppUnderlockTestUtil {

  // The status update trigger is paused so that it creates no instructions
  // of its own.
  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1SvWithSimTime(this.getClass.getSimpleName)
      .addConfigTransform((_, config) =>
        updateAutomationConfig(ConfigurableApp.Sv)(
          _.withPausedTrigger[ExecuteFeaturedAppUnderlockInstructionTrigger]
            .withPausedTrigger[FeaturedAppUnderlockStatusUpdateTrigger]
        )(config)
      )

  "suspend the right from execution time" in { implicit env =>
    val alice = grantRight(aliceWalletClient, aliceValidatorBackend)
    voteToCreateUnderlockInstruction(alice, underlocked = true)
    // Execution happens later than the instruction's creation,
    // we advance time to test that "underlock permanent at" is computed
    // from execution time but not creation time.
    advanceTimeWithinRound(Duration.ofMinutes(1))

    val deadline = executeUnderlockAndExpectPermanentAt(alice)
    featuredAppRightOf(alice).data.underlockPermanentAt.toScala shouldBe Some(deadline)
  }

  "keep the earlier deadline on a repeated underlock" in { implicit env =>
    val alice = grantRight(aliceWalletClient, aliceValidatorBackend)
    voteToCreateUnderlockInstruction(alice, underlocked = true)
    val deadline = executeUnderlockAndExpectPermanentAt(alice)
    val suspendedRight = featuredAppRightOf(alice)

    advanceTimeWithinRound(Duration.ofMinutes(1))
    voteToCreateUnderlockInstruction(alice, underlocked = true)
    runTriggerAndCheck(
      "the instruction is archived, and the right keeps its contract id " +
        "and its deadline"
    ) {
      underlockInstructionsFor(alice) shouldBe empty
      val right = featuredAppRightOf(alice)
      right.id shouldBe suspendedRight.id
      right.data.underlockPermanentAt.toScala shouldBe Some(deadline)
    }
  }

  "clear the deadline on recovery" in { implicit env =>
    val alice = grantRight(aliceWalletClient, aliceValidatorBackend)
    voteToCreateUnderlockInstruction(alice, underlocked = true)
    executeUnderlockAndExpectPermanentAt(alice)

    voteToCreateUnderlockInstruction(alice, underlocked = false)
    runTriggerAndCheck(
      "the instruction is archived and the right has no deadline"
    ) {
      underlockInstructionsFor(alice) shouldBe empty
      isSuspended(alice) shouldBe false
    }
    val recoveredRight = featuredAppRightOf(alice)

    voteToCreateUnderlockInstruction(alice, underlocked = false)
    runTriggerAndCheck(
      "a recovery of a right without a deadline archives the instruction " +
        "and keeps the right's contract id"
    ) {
      underlockInstructionsFor(alice) shouldBe empty
      featuredAppRightOf(alice).id shouldBe recoveredRight.id
    }
  }

  // TODO(canton-network/splice-sv-fa-locking#205): pause the trigger that
  // expires underlock instructions here, once it exists.
  "apply an instruction past its expiry" in { implicit env =>
    val alice = grantRight(aliceWalletClient, aliceValidatorBackend)
    // A short timeout lets the instruction expire within the current round.
    setVoteRequestTimeout(Duration.ofMinutes(1))
    voteToCreateUnderlockInstruction(alice, underlocked = true)
    val expiresAt = underlockInstructionsFor(alice).loneElement.data.expiresAt
    advanceTimeWithinRound(
      Duration.between(getLedgerTime.toInstant, expiresAt).plusMinutes(1)
    )

    executeUnderlockAndExpectPermanentAt(alice)
  }

  "leave an instruction without a right" in { implicit env =>
    val bob = onboardWalletUser(bobWalletClient, bobValidatorBackend)
    voteToCreateUnderlockInstruction(bob, underlocked = true)

    clue("the instruction is not sampled while bob has no right") {
      runTrigger() shouldBe false
      underlockInstructionsFor(bob) should have size 1
    }

    clue("the instruction applies to a right granted later") {
      grantRight(bobWalletClient, bobValidatorBackend)
      executeUnderlockAndExpectPermanentAt(bob)
    }
  }

  /** Advances sim time without reaching the next round's opening, so that the
    * round automation has no work to do: long jumps leave it catching up
    * while the test runs, which causes flakes (canton-network/splice#7223).
    */
  private def advanceTimeWithinRound(duration: Duration)(implicit
      env: SpliceTestConsoleEnvironment
  ): Unit = {
    val now = getLedgerTime.toInstant
    val nextOpening =
      sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
        .filterJava(OpenMiningRound.COMPANION)(dsoParty)
        .map(_.data.opensAt)
        .filter(_.isAfter(now))
        .min
    clue(s"advancing by $duration stays before $nextOpening") {
      now.plus(duration).isBefore(nextOpening) shouldBe true
    }
    advanceTime(duration)
  }

  /** Sets the DSO's vote request timeout, which is also the lifetime of new
    * underlock instructions. With one SV, the vote is accepted at once.
    */
  private def setVoteRequestTimeout(timeout: Duration)(implicit
      env: SpliceTestConsoleEnvironment
  ): Unit = {
    val config = sv1Backend.getDsoInfo().dsoRules.payload.config
    val voteRequestTimeout = new RelTime(timeout.toNanos / 1000)
    val newConfig = new DsoRulesConfig(
      config.numUnclaimedRewardsThreshold,
      config.numMemberTrafficContractsThreshold,
      config.actionConfirmationTimeout,
      config.svOnboardingRequestTimeout,
      config.svOnboardingConfirmedTimeout,
      voteRequestTimeout,
      config.dsoDelegateInactiveTimeout,
      config.synchronizerNodeConfigLimits,
      config.maxTextLength,
      config.decentralizedSynchronizer,
      config.nextScheduledSynchronizerUpgrade,
      config.voteCooldownTime,
      config.nextScheduledLogicalSynchronizerUpgrade,
      config.svOperationsSwitchOverTimes,
      config.governanceLockManagementConfig,
    )
    val action = new ARC_DsoRules(
      new SRARC_SetConfig(new DsoRules_SetConfig(newConfig, Optional.empty()))
    )
    actAndCheck(
      s"SV votes to set the vote request timeout to $timeout",
      sv1Backend.createVoteRequest(
        sv1Backend.getDsoInfo().svParty.toProtoPrimitive,
        action,
        "url",
        "vote request timeout",
        config.voteRequestTimeout,
        None,
      ),
    )(
      "the DSO rules carry the new timeout",
      _ =>
        sv1Backend
          .getDsoInfo()
          .dsoRules
          .payload
          .config
          .voteRequestTimeout shouldBe voteRequestTimeout,
    )
  }

  private def runTrigger()(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.dsoDelegateBasedAutomation
      .trigger[ExecuteFeaturedAppUnderlockInstructionTrigger]
      .runOnce()
      .futureValue

  /** Runs the trigger on the provider's only instruction, which must say
    * `underlocked = True`, and checks that it suspended the provider's
    * right. Returns the expected time when the underlock becomes permanent:
    * now plus the grace period
    */
  private def executeUnderlockAndExpectPermanentAt(provider: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): Instant = {
    underlockInstructionsFor(provider).loneElement.data.underlocked shouldBe true
    // Sim time stands still until advanced, so this is the execution time.
    val deadline = getLedgerTime.toInstant.plus(gracePeriod)
    runTriggerAndCheck(
      "the instruction is archived and the right is suspended"
    ) {
      underlockInstructionsFor(provider) shouldBe empty
      isSuspended(provider) shouldBe true
    }
    deadline
  }

  /** Runs the trigger once, expecting it to process a task, and waits until
    * `check` holds.
    */
  private def runTriggerAndCheck(description: String)(check: => Unit)(implicit
      env: SpliceTestConsoleEnvironment
  ): Unit = {
    actAndCheck("the execute trigger runs", runTrigger() shouldBe true)(
      description,
      _ => check,
    )
    ()
  }

  private def gracePeriod(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend
      .getDsoInfo()
      .dsoRules
      .payload
      .config
      .governanceLockManagementConfig
      .toScala
      .flatMap(_.featuredAppUnderlockGracePeriod.toScala)
      .map(relTime => Duration.ofNanos(relTime.microseconds * 1000))
      .getOrElse(Duration.ofDays(7))

  private def grantRight(
      wallet: WalletAppClientReference,
      validator: ValidatorAppReference,
  )(implicit env: SpliceTestConsoleEnvironment): PartyId = {
    val provider = onboardWalletUser(wallet, validator)
    grantFeaturedAppRight(wallet)
    provider
  }

  /** With one SV, the vote request is accepted at once and creates the
    * instruction, which expires after `voteRequestTimeout`.
    */
  private def voteToCreateUnderlockInstruction(
      provider: PartyId,
      underlocked: Boolean,
  )(implicit env: SpliceTestConsoleEnvironment): Unit = {
    val action = new ARC_DsoRules(
      new SRARC_CreateFeaturedAppUnderlockInstruction(
        new DsoRules_CreateFeaturedAppUnderlockInstruction(
          provider.toProtoPrimitive,
          underlocked,
        )
      )
    )
    actAndCheck(
      s"SV votes for an underlock instruction (underlocked = $underlocked)",
      sv1Backend.createVoteRequest(
        sv1Backend.getDsoInfo().svParty.toProtoPrimitive,
        action,
        "url",
        "underlock instruction",
        sv1Backend.getDsoInfo().dsoRules.payload.config.voteRequestTimeout,
        None,
      ),
    )(
      "the instruction is created",
      _ =>
        underlockInstructionsFor(provider).loneElement.data.underlocked shouldBe
          underlocked,
    )
  }
}
