package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.IntegrationTest
import org.lfdecentralizedtrust.splice.util.*

import scala.concurrent.duration.*

@org.lfdecentralizedtrust.splice.util.scalatesttags.SpliceDsoGovernance_0_1_32
class FeaturedAppUnderlockStatusUpdateIntegrationTest
    extends IntegrationTest
    with WalletTestUtil
    with GovernanceLockTest
    with FeaturedAppUnderlockTest {

  // The smallest amount a lock can have, so a single lock is enough to meet the threshold
  private val basicLockThreshold = minimumLockAmount

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1Sv(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        withFeaturedAppUnderlockEnforcement(basicLockThreshold = Some(basicLockThreshold))(config)
      )
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAllSvAppConfigs_(
          _.copy(
            featuredAppUnderlockStatusUpdateInterval = NonNegativeFiniteDuration.ofSeconds(1)
          )
        )(config)
      )

  "create a single underlock instruction per featured app provider without locks" in {
    implicit env =>
      val alice = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      val bob = onboardWalletUser(bobWalletClient, bobValidatorBackend)

      val (_, instructionsSeen) = actAndCheck(
        "alice and bob grant themselves a FeaturedAppRight without locking anything",
        Seq(aliceWalletClient, bobWalletClient).foreach(client =>
          eventuallySucceeds() {
            client.selfGrantFeaturedAppRight()
          }
        ),
      )(
        "the SVs record alice and bob as underlocked",
        _ =>
          Seq(alice, bob).map { provider =>
            // The instruction is either awaiting execution, or already executed and suspended the right
            val instructions = instructionsFor(provider)
            instructions.foreach(_.data.underlocked shouldBe true)
            if (instructions.isEmpty) isSuspended(provider) shouldBe true
            provider -> instructions.size
          },
      )

      clue("no duplicate instructions are created") {
        always(10.seconds, pollIntervalMs = 1000) {
          instructionsSeen.foreach { case (provider, seen) =>
            val numInstructions = instructionsFor(provider).size
            // Once executed, the right records the underlock, so no new instruction may appear
            if (seen == 0) numInstructions shouldBe 0 else numInstructions should be <= 1
          }
        }
      }
  }

  "lift the suspension once a provider locks enough" in { implicit env =>
    val charlie = onboardWalletUser(charlieWalletClient, aliceValidatorBackend)
    charlieWalletClient.tap(basicLockThreshold)

    actAndCheck(
      "charlie grants himself a FeaturedAppRight without locking anything",
      eventuallySucceeds() {
        charlieWalletClient.selfGrantFeaturedAppRight()
      },
    )(
      "charlie's right is suspended",
      _ => {
        executeInstructionsFor(charlie)
        isSuspended(charlie) shouldBe true
      },
    )

    actAndCheck(
      "charlie locks enough for a basic featured app",
      // The lock starts out provisional, and is converted to a full lock as charlie has a right
      tsv1Operations.createGovernanceLock(
        aliceValidatorBackend.participantClientWithAdminToken,
        RichPartyId.local(charlie),
        lockKind = ProvisionalFeaturedAppLock,
        lockSubject = charlie.toProtoPrimitive,
        amount = basicLockThreshold,
      ),
    )(
      "charlie's suspension is lifted",
      _ => {
        executeInstructionsFor(charlie)
        isSuspended(charlie) shouldBe false
      },
    )
  }
}
