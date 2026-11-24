// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.HasExecutionContext
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.DsoRules_CreateFeaturedAppUnderlockInstruction
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.actionrequiringconfirmation.ARC_DsoRules
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.dsorules_actionrequiringconfirmation.SRARC_CreateFeaturedAppUnderlockInstruction
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
import org.lfdecentralizedtrust.splice.store.PageLimit
import org.lfdecentralizedtrust.splice.sv.automation.confirmation.FeaturedAppUnderlockStatusUpdateTrigger
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.ExecuteFeaturedAppUnderlockInstructionTrigger
import org.lfdecentralizedtrust.splice.util.{TimeTestUtil, WalletTestUtil}

import java.time.{Duration, Instant}
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
      provider,
      "the instruction is archived and the right is suspended",
    ) {
      underlockInstructionsFor(provider) shouldBe empty
      isSuspended(provider) shouldBe true
    }
    deadline
  }

  /** Waits until the SV store samples the provider's only instruction
    * paired with its right, runs the trigger once, expecting it to process
    * a task, and waits until `check` holds.
    */
  private def runTriggerAndCheck(provider: PartyId, description: String)(
      check: => Unit
  )(implicit env: SpliceTestConsoleEnvironment): Unit = {
    clue("the SV store samples the instruction paired with the right") {
      // The trigger reads the SV store, which ingests after the participant
      // with a lag. Hence, we wait for the instruction and the right to appear
      // when we sample the store.
      eventually() {
        val expected =
          underlockInstructionsFor(provider).loneElement.id ->
            featuredAppRightOf(provider).id
        val sampled = sv1Backend.appState.dsoStore
          .listFeaturedAppUnderlockInstructionSample(PageLimit.tryCreate(10))
          .futureValue
          .map(task => task.instruction.contractId -> task.featuredAppRightCid)
        sampled should contain(expected)
      }
    }
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
