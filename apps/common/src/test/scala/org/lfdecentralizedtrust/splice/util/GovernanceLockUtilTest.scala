package org.lfdecentralizedtrust.splice.util

import cats.data.NonEmptyList
import com.digitalasset.canton.BaseTest
import com.digitalasset.canton.topology.PartyId
import io.circe.syntax.*
import org.lfdecentralizedtrust.splice.codegen.java.da.time.types.RelTime
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.LockedAmulet
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock as gl
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import java.util.Optional
import scala.jdk.CollectionConverters.*

class GovernanceLockUtilTest extends AnyWordSpec with BaseTest {
  private val glu = GovernanceLockUtil

  private val provider = "provider::1220aaaa"
  private val dso = "dso::1220dddd"
  private val owner = "owner::1220bbbb"
  private val targetOwner = "target::1220cccc"
  private val alice = "alice::1220alice"
  private val bob = "bob::1220bob"
  private val charlie = "charlie::1220charlie"
  private val featuredApp = new gl.governancelockkind.GLK_FeaturedApp(provider)
  private val provisionalFeaturedApp =
    new gl.governancelockkind.GLK_ProvisionalFeaturedApp(provider)
  private val superValidator = new gl.governancelockkind.GLK_SuperValidatorRightsOwner("sv1")

  private def controllers(
      cs: List[List[String]]
  ): Optional[gl.controllers.ControllersSpecification] =
    Optional.of(new gl.controllers.ControllersSpecification(cs.map(_.asJava).asJava))

  private val unlockControllers = controllers(List(List(alice, bob)))
  private val substitutionControllers = controllers(List(List(bob, charlie)))
  private val vestingControllers = controllers(List(List(alice, charlie)))

  private def fullSpec(kind: gl.GovernanceLockKind): gl.FullLockSpecification =
    new gl.FullLockSpecification(
      kind,
      Optional.of(
        new gl.controllers.GovernanceLockControllers(
          unlockControllers,
          substitutionControllers,
          vestingControllers,
        )
      ),
    )

  private def vestingSpec(kind: gl.GovernanceLockKind): gl.VestingLockSpecification =
    new gl.VestingLockSpecification(
      kind,
      new RelTime(1_000_000),
      Instant.EPOCH,
      vestingControllers,
    )

  private def spec(kind: gl.GovernanceLockKind, vesting: Boolean): gl.GovernanceLockSpecification =
    if (vesting) new gl.governancelockspecification.GLS_VestingLock(vestingSpec(kind))
    else new gl.governancelockspecification.GLS_FullLock(fullSpec(kind))

  "specificationTypeAndSubject" should {
    "encode every kind of full lock" in {
      glu.specificationTypeAndSubject(spec(featuredApp, false)) shouldBe
        (glu.StoreTypeAndKind(glu.StoreType.Full, glu.StoreKind.FeaturedApp), provider)
      glu.specificationTypeAndSubject(spec(provisionalFeaturedApp, false)) shouldBe
        (glu.StoreTypeAndKind(glu.StoreType.Full, glu.StoreKind.ProvisionalFeaturedApp), provider)
      glu.specificationTypeAndSubject(spec(superValidator, false)) shouldBe
        (glu.StoreTypeAndKind(glu.StoreType.Full, glu.StoreKind.SuperValidator), "sv1")
    }

    "encode every kind of vesting lock" in {
      glu.specificationTypeAndSubject(spec(featuredApp, true)) shouldBe
        (glu.StoreTypeAndKind(glu.StoreType.Vesting, glu.StoreKind.FeaturedApp), provider)
      glu.specificationTypeAndSubject(spec(provisionalFeaturedApp, true)) shouldBe
        (glu.StoreTypeAndKind(
          glu.StoreType.Vesting,
          glu.StoreKind.ProvisionalFeaturedApp,
        ), provider)
      glu.specificationTypeAndSubject(spec(superValidator, true)) shouldBe
        (glu.StoreTypeAndKind(glu.StoreType.Vesting, glu.StoreKind.SuperValidator), "sv1")
    }
  }

  private def lock(
      kind: gl.GovernanceLockKind,
      vesting: Boolean,
      lockOwner: String,
  ): gl.GovernanceLock =
    new gl.GovernanceLock(
      dso,
      lockOwner,
      new java.math.BigDecimal(10),
      new LockedAmulet.ContractId("cid"),
      spec(kind, vesting),
      Optional.empty(),
      Instant.EPOCH,
    )

  private def substitution(
      kind: gl.GovernanceLockKind,
      vesting: Boolean,
      proposers: NonEmptyList[String],
  ): gl.substitution.GovernanceLockSubstitution =
    new gl.substitution.GovernanceLockSubstitution(
      new gl.GovernanceLockKey(dso, targetOwner, spec(kind, vesting)),
      proposers.head,
      new java.math.BigDecimal(10),
      Optional.empty(),
      Instant.EPOCH,
      Instant.EPOCH,
      Optional.empty(),
      Optional.of(proposers.tail.asJava),
      Optional.empty(),
      Optional.empty(),
    )

  private def transfer(
      kind: gl.GovernanceLockKind,
      vesting: Boolean,
      receiver: String,
      proposers: List[String],
  ): gl.transfer.GovernanceLockTransfer =
    new gl.transfer.GovernanceLockTransfer(
      new gl.GovernanceLockKey(dso, owner, spec(kind, vesting)),
      receiver,
      new java.math.BigDecimal(10),
      Instant.EPOCH,
      Instant.EPOCH,
      if (proposers.nonEmpty) Optional.of(proposers.asJava) else Optional.empty(),
      Optional.empty(),
      Optional.empty(),
      Optional.empty(),
    )

  private def unlock(
      kind: gl.GovernanceLockKind,
      proposers: List[String],
  ): gl.unlock.UnlockProposal =
    new gl.unlock.UnlockProposal(
      new gl.unlock.UnlockProposalSpecification(
        dso,
        owner,
        fullSpec(kind),
        new java.math.BigDecimal(10),
        Instant.EPOCH,
      ),
      proposers.asJava,
      Map.empty.asJava,
      Instant.EPOCH,
      Instant.EPOCH,
      Optional.empty(),
    )

  private def withdraw(
      kind: gl.GovernanceLockKind,
      proposers: List[String],
  ): gl.withdraw.WithdrawVestedFundsProposal =
    new gl.withdraw.WithdrawVestedFundsProposal(
      new gl.withdraw.WithdrawVestedFundsSpecification(
        dso,
        owner,
        vestingSpec(kind),
        Instant.EPOCH,
      ),
      proposers.asJava,
      Map.empty.asJava,
      Instant.EPOCH,
      Instant.EPOCH,
      Optional.empty(),
    )

  "governanceLockInformees lists and deduplicates the correct parties" in {
    def expectedCustomControllers(vesting: Boolean): Seq[String] =
      if (vesting) Seq(alice, charlie) else Seq(alice, bob, charlie)

    Seq(owner, provider).foreach { owner =>
      Seq(true, false).foreach { sv =>
        val (kind, expectedProvider) =
          if (sv) (superValidator, None) else (featuredApp, Some(provider))

        Seq(true, false).foreach { vesting =>
          withClue(s"owner: $owner, sv: $sv, vesting: $vesting") {
            glu.governanceLockInformees(lock(kind, vesting, owner)) shouldBe
              (Seq(owner) ++ expectedProvider ++ expectedCustomControllers(vesting)).distinct
          }
        }
      }
    }
  }

  "substitutionInformees lists and deduplicates the correct parties" in {
    def expectedCustomControllers(vesting: Boolean): Seq[String] =
      if (vesting) Seq(alice, charlie) else Seq(bob, charlie)

    Seq(
      NonEmptyList.one(owner),
      NonEmptyList.one(targetOwner),
      NonEmptyList.of(owner, targetOwner, alice, bob, charlie),
    ).foreach { proposers =>
      Seq(true, false).foreach { sv =>
        val (kind, expectedProvider) =
          if (sv) (superValidator, None) else (featuredApp, Some(provider))

        Seq(true, false).foreach { vesting =>
          withClue(s"proposers: $proposers, sv: $sv, vesting: $vesting") {
            glu.substitutionInformees(substitution(kind, vesting, proposers)) shouldBe
              (Seq(proposers.head, targetOwner) ++
                expectedProvider ++
                proposers.tail ++
                expectedCustomControllers(vesting)).distinct
          }
        }
      }
    }
  }

  "transferInformees lists and deduplicates the correct parties" in {
    def expectedCustomControllers(vesting: Boolean): Seq[String] =
      if (vesting) Seq(alice, charlie) else Seq(bob, charlie)

    Seq(targetOwner, alice).foreach { receiver =>
      Seq(
        List.empty[String],
        List(receiver),
        List(receiver, alice, bob, charlie),
      ).foreach { proposers =>
        Seq(true, false).foreach { sv =>
          val kind = if (sv) superValidator else featuredApp

          Seq(true, false).foreach { vesting =>
            withClue(s"receiver: $receiver, proposers: $proposers, sv: $sv, vesting: $vesting") {
              glu.transferInformees(transfer(kind, vesting, receiver, proposers)) shouldBe
                (Seq(owner, receiver) ++ proposers ++ expectedCustomControllers(vesting)).distinct
            }
          }
        }
      }
    }
  }

  "unlockInformees lists and deduplicates the correct parties" in {
    val expectedCustomControllers = Seq(alice, bob)

    Seq(owner, charlie).foreach { proposer =>
      Seq(true, false).foreach { sv =>
        val (kind, expectedProvider) =
          if (sv) (superValidator, None) else (featuredApp, Some(provider))

        withClue(s"proposer: $proposer, sv: $sv") {
          glu.unlockInformees(unlock(kind, List(proposer))) shouldBe
            (Seq(owner, proposer) ++
              expectedProvider ++
              expectedCustomControllers).distinct
        }
      }
    }
  }

  "withdrawInformees lists and deduplicates the correct parties" in {
    val expectedCustomControllers = Seq(alice, charlie)

    Seq(owner, bob).foreach { proposer =>
      Seq(true, false).foreach { sv =>
        val (kind, expectedProvider) =
          if (sv) (superValidator, None) else (featuredApp, Some(provider))

        withClue(s"proposer: $proposer, sv: $sv") {
          glu.withdrawInformees(withdraw(kind, List(proposer))) shouldBe
            (Seq(owner, proposer) ++
              expectedProvider ++
              expectedCustomControllers).distinct
        }
      }
    }
  }

  "Memo.parseTarget" should {
    import glu.Memo.*

    val dsoParty = PartyId.tryFromProtoPrimitive(dso)
    val senderParty = PartyId.tryFromProtoPrimitive(owner)

    def memo(fields: Seq[(String, String)]): String =
      glu.memoPrefix + fields.map { case (k, v) => s"$k=$v" }.mkString("&")

    def parse(fields: Seq[(String, String)]): Either[String, Option[gl.GovernanceLockKey]] =
      parseTarget(dsoParty, senderParty, memo(fields))

    def json(cs: List[List[String]]): String = cs.asJson.noSpaces

    def without(fields: Seq[(String, String)], key: String): Seq[(String, String)] =
      fields.filterNot(_._1 == key)

    def updated(fields: Seq[(String, String)], key: String, value: String): Seq[(String, String)] =
      without(fields, key) :+ (key -> value)

    def key(lockOwner: String, specification: gl.GovernanceLockSpecification) =
      new gl.GovernanceLockKey(dso, lockOwner, specification)

    // Controllers are given unsorted and with duplicates to test that they are normalized
    def fullLockFields(kind: glu.LockKind, subject: String): Seq[(String, String)] =
      Seq(
        lockKindKey -> kind.kind,
        lockSubjectKey -> subject,
        unlockControllersKey -> json(List(List(bob, alice, alice), List(alice, bob))),
        substitutionControllersKey -> json(List(List(charlie, bob))),
        vestingControllersKey -> json(List(List(alice, charlie))),
      )

    def vestingLockFields(kind: glu.LockKind, subject: String): Seq[(String, String)] =
      Seq(
        lockStatusKey -> glu.LockStatus.Vesting.status,
        lockKindKey -> kind.kind,
        lockSubjectKey -> subject,
        vestingDurationKey -> "1000000",
        vestingEndTimeKey -> "0",
        vestingControllersKey -> json(List(List(charlie, alice), List(alice, charlie))),
      )

    "parse a create-lock request" in {
      parse(Seq(requestKey -> createLockRequest)) shouldBe Right(None)
    }

    "parse an unlock request" in {
      parse(
        Seq(requestKey -> unlockRequest) ++ fullLockFields(glu.LockKind.FeaturedApp, provider)
      ) shouldBe Right(Some(key(owner, spec(featuredApp, false))))
    }

    "parse a transfer request" in {
      withClue("default lock status") {
        parse(
          Seq(requestKey -> transferRequest) ++
            fullLockFields(glu.LockKind.SuperValidator, "sv1")
        ) shouldBe Right(Some(key(owner, spec(superValidator, false))))
      }
      withClue("locked lock status with target lock owner") {
        parse(
          Seq(
            requestKey -> transferRequest,
            lockStatusKey -> glu.LockStatus.Locked.status,
            targetLockOwnerKey -> targetOwner,
          ) ++ fullLockFields(glu.LockKind.FeaturedApp, provider)
        ) shouldBe Right(Some(key(targetOwner, spec(featuredApp, false))))
      }
      withClue("vesting lock status") {
        parse(
          Seq(requestKey -> transferRequest) ++
            vestingLockFields(glu.LockKind.ProvisionalFeaturedApp, provider)
        ) shouldBe Right(Some(key(owner, spec(provisionalFeaturedApp, true))))
      }
    }

    "parse a withdraw request" in {
      parse(
        Seq(requestKey -> withdrawRequest, targetLockOwnerKey -> targetOwner) ++
          vestingLockFields(glu.LockKind.SuperValidator, "sv1")
      ) shouldBe Right(Some(key(targetOwner, spec(superValidator, true))))
    }

    "parse a substitute request" in {
      withClue("full lock") {
        parse(
          Seq(requestKey -> substituteRequest, targetLockOwnerKey -> targetOwner) ++
            fullLockFields(glu.LockKind.ProvisionalFeaturedApp, provider)
        ) shouldBe Right(Some(key(targetOwner, spec(provisionalFeaturedApp, false))))
      }
      withClue("vesting lock") {
        parse(
          Seq(requestKey -> substituteRequest, targetLockOwnerKey -> targetOwner) ++
            vestingLockFields(glu.LockKind.FeaturedApp, provider)
        ) shouldBe Right(Some(key(targetOwner, spec(featuredApp, true))))
      }
    }

    "fail on invalid memos" in {
      val validFull =
        Seq(requestKey -> transferRequest) ++ fullLockFields(glu.LockKind.FeaturedApp, provider)
      val validVesting =
        Seq(requestKey -> transferRequest) ++ vestingLockFields(glu.LockKind.FeaturedApp, provider)

      Seq(
        (
          "field without value",
          memo(validFull) + "&foo",
          "Expected `key=value`, got: foo",
        ),
        (
          "field with multiple values",
          memo(validFull) + "&a=b=c",
          "Expected `key=value`, got: a=b=c",
        ),
        (
          "missing request",
          memo(without(validFull, requestKey)),
          s"Missing required memo key: '$requestKey'",
        ),
        (
          "unknown request",
          memo(updated(validFull, requestKey, "foo")),
          "Unknown request type: 'foo'",
        ),
        (
          "missing lock subject",
          memo(without(validFull, lockSubjectKey)),
          s"Missing required memo key: '$lockSubjectKey'",
        ),
        (
          "missing lock kind",
          memo(without(validFull, lockKindKey)),
          s"Missing required memo key: '$lockKindKey'",
        ),
        (
          "unknown lock kind",
          memo(updated(validFull, lockKindKey, "foo")),
          "Unknown lock kind: 'foo'",
        ),
        (
          "unknown lock status",
          memo(updated(validFull, lockStatusKey, "foo")),
          "Unknown lock status: 'foo'",
        ),
        (
          "missing vesting duration",
          memo(without(validVesting, vestingDurationKey)),
          s"Missing required memo key: '$vestingDurationKey'",
        ),
        (
          "non-numeric vesting duration",
          memo(updated(validVesting, vestingDurationKey, "foo")),
          s"Failed to parse memo key '$vestingDurationKey' as Long: 'foo'",
        ),
        (
          "missing target lock owner",
          memo(Seq(requestKey -> substituteRequest) ++ without(validFull, requestKey)),
          s"Missing required memo key: '$targetLockOwnerKey'",
        ),
      ).foreach { case (description, memoString, expectedError) =>
        withClue(description) {
          parseTarget(dsoParty, senderParty, memoString) shouldBe Left(expectedError)
        }
      }

      withClue("invalid controllers") {
        parse(updated(validFull, unlockControllersKey, "foo")) match {
          case Left(error) => error should startWith(s"Error in memo key '$unlockControllersKey': ")
          case Right(result) => fail(s"Expected failure, got: $result")
        }
      }
    }
  }
}
