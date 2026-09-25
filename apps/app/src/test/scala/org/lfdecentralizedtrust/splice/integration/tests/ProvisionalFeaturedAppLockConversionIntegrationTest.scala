package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.topology.PartyId
import com.digitalasset.canton.discard.Implicits.DiscardOps
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.LockedAmulet
import org.lfdecentralizedtrust.splice.codegen.java.splice.api.token.metadatav1.Metadata
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock
import org.lfdecentralizedtrust.splice.config.ConfigTransforms
import org.lfdecentralizedtrust.splice.integration.EnvironmentDefinition
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.{
  IntegrationTest,
  SpliceTestConsoleEnvironment,
}
import org.lfdecentralizedtrust.splice.sv.automation.delegatebased.ProvisionalFeaturedAppLockConversionTrigger
import org.lfdecentralizedtrust.splice.util.*

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.Optional

class ProvisionalFeaturedAppLockConversionIntegrationTest
    extends IntegrationTest
    with WalletTestUtil
    with TriggerTestUtil {

  override def environmentDefinition: SpliceEnvironmentDefinition =
    EnvironmentDefinition
      .simpleTopology1Sv(this.getClass.getSimpleName)
      .addConfigTransforms((_, config) =>
        ConfigTransforms.updateAutomationConfig(ConfigTransforms.ConfigurableApp.Sv)(
          _.withPausedTrigger[ProvisionalFeaturedAppLockConversionTrigger]
        )(config)
      )

  private def conversionTrigger(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.dsoDelegateBasedAutomation.trigger[ProvisionalFeaturedAppLockConversionTrigger]

  private def createProvisionalLock(owner: PartyId, provider: PartyId)(implicit
      env: SpliceTestConsoleEnvironment
  ): Unit =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.commands
      .submitWithResult(
        userId = sv1Backend.config.ledgerApiUser,
        actAs = Seq(dsoParty, owner),
        readAs = Seq.empty,
        update = new governancelock.GovernanceLock(
          dsoParty.toProtoPrimitive,
          owner.toProtoPrimitive,
          BigDecimal(10000).bigDecimal,
          new LockedAmulet.ContractId("00" * 33 + "01"),
          new governancelock.GovernanceLockSpecification(
            new governancelock.governancelockkind.GLK_ProvisionalFeaturedApp(
              provider.toProtoPrimitive
            )
          ),
          Optional.empty(),
          Instant.now().truncatedTo(ChronoUnit.MICROS),
          new Metadata(java.util.Collections.emptyMap()),
        ).create(),
      )
      .discard

  private def locksFor(provider: PartyId)(implicit env: SpliceTestConsoleEnvironment) =
    sv1Backend.participantClientWithAdminToken.ledger_api_extensions.acs
      .filterJava(governancelock.GovernanceLock.COMPANION)(
        dsoParty,
        _.data.specification.kind match {
          case k: governancelock.governancelockkind.GLK_ProvisionalFeaturedApp =>
            k.provider == provider.toProtoPrimitive
          case k: governancelock.governancelockkind.GLK_FeaturedApp =>
            k.provider == provider.toProtoPrimitive
          case _ => false
        },
      )

  "convert a provisional featured app lock once the provider has a FeaturedAppRight" in {
    implicit env =>
      val provider = onboardWalletUser(aliceWalletClient, aliceValidatorBackend)
      val owner = sv1Backend.getDsoInfo().svParty

      eventuallySucceeds() {
        aliceWalletClient.selfGrantFeaturedAppRight()
      }

      createProvisionalLock(owner, provider)

      clue("the lock starts out provisional") {
        locksFor(provider).loneElement.data.specification.kind shouldBe
          new governancelock.governancelockkind.GLK_ProvisionalFeaturedApp(
            provider.toProtoPrimitive
          )
      }

      setTriggersWithin(triggersToResumeAtStart = Seq(conversionTrigger)) {
        eventually() {
          locksFor(provider).loneElement.data.specification.kind shouldBe
            new governancelock.governancelockkind.GLK_FeaturedApp(provider.toProtoPrimitive)
        }
      }
  }

  "leave a provisional lock alone when the provider has no FeaturedAppRight" in { implicit env =>
    val provider = onboardWalletUser(bobWalletClient, bobValidatorBackend)
    val owner = sv1Backend.getDsoInfo().svParty

    createProvisionalLock(owner, provider)

    setTriggersWithin(triggersToResumeAtStart = Seq(conversionTrigger)) {
      conversionTrigger.runOnce().futureValue

      locksFor(provider).loneElement.data.specification.kind shouldBe
        new governancelock.governancelockkind.GLK_ProvisionalFeaturedApp(
          provider.toProtoPrimitive
        )
    }
  }
}
