// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.util

import cats.syntax.either.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import com.digitalasset.canton.topology.PartyId
import io.circe.parser.decode
import io.grpc.Status
import org.lfdecentralizedtrust.splice.codegen.java.da.time.types.RelTime
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock as gl

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.Optional
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

object GovernanceLockUtil {
  val cipPrefix = "cip-127"
  val memoPrefix = s"$cipPrefix/memo:"

  /** Receiver of all governance lock requests, mirroring `lockManagerParty` in `Splice.GovernanceLock`. */
  val lockManagerParty = PartyId.tryFromProtoPrimitive(
    "cip-127_lock-manager::1220000000000000000000000000000000000000000000000000000000000000abcd"
  )

  sealed abstract class LockKind(val kind: String)
  object LockKind {
    case object SuperValidator extends LockKind("sv-lock")
    case object FeaturedApp extends LockKind("fa-lock")
    case object ProvisionalFeaturedApp extends LockKind("provisional-fa-lock")
  }

  sealed abstract class LockStatus(val status: String)
  object LockStatus {
    case object Locked extends LockStatus("locked")
    case object Vesting extends LockStatus("vesting")
  }

  def foldSpecification[A](specification: gl.GovernanceLockSpecification)(
      onFull: gl.FullLockSpecification => A,
      onVesting: gl.VestingLockSpecification => A,
  ): A =
    specification match {
      case s: gl.governancelockspecification.GLS_FullLock => onFull(s.fullLockSpecificationValue)
      case s: gl.governancelockspecification.GLS_VestingLock =>
        onVesting(s.vestingLockSpecificationValue)
      case s =>
        throw Status.INTERNAL
          .withDescription(s"Unexpected GovernanceLockSpecification: $s")
          .asRuntimeException()
    }

  def getSpecificationKind(specification: gl.GovernanceLockSpecification): gl.GovernanceLockKind =
    foldSpecification(specification)(_.kind, _.kind)

  def getGovernanceLockKind(lock: gl.GovernanceLock): gl.GovernanceLockKind =
    getSpecificationKind(lock.specification)

  def getVestingLockSpecification(
      specification: gl.GovernanceLockSpecification
  ): Option[gl.VestingLockSpecification] =
    foldSpecification(specification)(_ => None, Some(_))

  def getVestingLockSpecification(lock: gl.GovernanceLock): Option[gl.VestingLockSpecification] =
    getVestingLockSpecification(lock.specification)

  /** Parties observing a lock based on its kind, mirroring `governanceLockObservers`
    * in `Splice.GovernanceLock`.
    */
  private def governanceLockKindObservers(spec: gl.GovernanceLockSpecification): Seq[String] =
    getSpecificationKind(spec) match {
      case kind: gl.governancelockkind.GLK_FeaturedApp => Seq(kind.provider)
      case kind: gl.governancelockkind.GLK_ProvisionalFeaturedApp => Seq(kind.provider)
      case _ => Seq.empty
    }

  private def controllersSpecificationObservers(
      controllers: Optional[gl.controllers.ControllersSpecification]
  ): Seq[String] =
    controllers.toScala
      .fold(Seq.empty[String])(_.controllers.asScala.flatMap(_.asScala).toSeq)
      .distinct

  private def governanceLockControllersObservers(
      controllers: Optional[gl.controllers.GovernanceLockControllers],
      get: gl.controllers.GovernanceLockControllers => Optional[
        gl.controllers.ControllersSpecification
      ],
  ): Seq[String] =
    controllers.toScala.fold(Seq.empty[String])(cs => controllersSpecificationObservers(get(cs)))

  def governanceLockInformees(lock: gl.GovernanceLock): Seq[String] =
    (lock.owner +:
      governanceLockKindObservers(lock.specification) ++:
      foldSpecification(lock.specification)(
        spec =>
          governanceLockControllersObservers(spec.controllers, _.unlockControllers) ++
            governanceLockControllersObservers(spec.controllers, _.substitutionControllers) ++
            governanceLockControllersObservers(spec.controllers, _.vestingControllers),
        spec => controllersSpecificationObservers(spec.vestingControllers),
      )).distinct

  private def substitutionControllers(key: gl.GovernanceLockKey): Seq[String] =
    foldSpecification(key.specification)(
      spec => governanceLockControllersObservers(spec.controllers, _.substitutionControllers),
      spec => controllersSpecificationObservers(spec.vestingControllers),
    )

  def substitutionInformees(substitution: gl.substitution.GovernanceLockSubstitution): Seq[String] =
    (substitution.owner +:
      substitution.target.owner +:
      governanceLockKindObservers(substitution.target.specification) ++:
      substitution.proposers.toScala.fold(Seq.empty[String])(_.asScala.toSeq) ++:
      substitutionControllers(substitution.target)).distinct

  def transferInformees(transfer: gl.transfer.GovernanceLockTransfer): Seq[String] =
    (transfer.source.owner +:
      transfer.receiver +:
      transfer.proposers.toScala.fold(Seq.empty[String])(_.asScala.toSeq) ++:
      substitutionControllers(transfer.source)).distinct

  def unlockInformees(unlock: gl.unlock.UnlockProposal): Seq[String] =
    (unlock.specification.owner +:
      unlock.proposers.asScala.toSeq ++:
      governanceLockKindObservers(
        new gl.governancelockspecification.GLS_FullLock(unlock.specification.fullSpec)
      ) ++:
      governanceLockControllersObservers(
        unlock.specification.fullSpec.controllers,
        _.unlockControllers,
      )).distinct

  def withdrawInformees(withdraw: gl.withdraw.WithdrawVestedFundsProposal): Seq[String] =
    (withdraw.specification.owner +:
      withdraw.proposers.asScala.toSeq ++:
      governanceLockKindObservers(
        new gl.governancelockspecification.GLS_VestingLock(withdraw.specification.vestingSpec)
      ) ++:
      controllersSpecificationObservers(
        withdraw.specification.vestingSpec.vestingControllers
      )).distinct

  sealed abstract class StoreType(val type_ : String)
  object StoreType {
    case object Full extends StoreType("f")
    case object Vesting extends StoreType("v")
  }

  sealed abstract class StoreKind(val kind: String)
  object StoreKind {
    case object FeaturedApp extends StoreKind("fa")
    case object ProvisionalFeaturedApp extends StoreKind("pfa")
    case object SuperValidator extends StoreKind("sv")
  }

  final case class StoreTypeAndKind(type_ : StoreType, kind: StoreKind) {
    final val storeValue: String = s"${type_.type_}:${kind.kind}"
  }
  object StoreTypeAndKind {
    val provisionalFeaturedApp = StoreTypeAndKind(StoreType.Full, StoreKind.ProvisionalFeaturedApp)
    val fullFeaturedApp = StoreTypeAndKind(StoreType.Full, StoreKind.FeaturedApp)
  }

  /** Encodes a lock as the `governance_lock_type` and `governance_lock_subject` index columns. */
  def specificationTypeAndSubject(
      specification: gl.GovernanceLockSpecification
  ): (StoreTypeAndKind, String) = {
    val storeType =
      foldSpecification[StoreType](specification)(_ => StoreType.Full, _ => StoreType.Vesting)
    val (storeKind, subject) = getSpecificationKind(specification) match {
      case kind: gl.governancelockkind.GLK_FeaturedApp =>
        // The type annotation is necessary to avoid a wartremover error that says
        // Inferred type containing Product
        (StoreKind.FeaturedApp: StoreKind, kind.provider)
      case kind: gl.governancelockkind.GLK_ProvisionalFeaturedApp =>
        (StoreKind.ProvisionalFeaturedApp, kind.provider)
      case kind: gl.governancelockkind.GLK_SuperValidatorRightsOwner =>
        (StoreKind.SuperValidator, kind.svName)
      case kind =>
        throw Status.INTERNAL
          .withDescription(s"Unexpected GovernanceLockKind: $kind")
          .asRuntimeException()
    }
    (StoreTypeAndKind(storeType, storeKind), subject)
  }

  def governanceLockTypeAndSubject(lock: gl.GovernanceLock): (StoreTypeAndKind, String) =
    specificationTypeAndSubject(lock.specification)

  object Memo {
    type Fields = Map[String, String]

    // Request types
    val createLockRequest = "create-lock"
    val unlockRequest = "unlock-and-start-vesting"
    val transferRequest = "transfer-lock"
    val withdrawRequest = "withdraw-vested-funds"
    val substituteRequest = "substitute-lock"

    // Memo keys
    val requestKey = "request"
    val lockKindKey = "lock-kind"
    val lockStatusKey = "lock-status"
    val lockSubjectKey = "lock-subject"
    val targetLockOwnerKey = "target-lock-owner"
    val vestingDurationKey = "lock-vesting-duration-micros"
    val vestingEndTimeKey = "lock-vesting-end-time-micros"
    val unlockControllersKey = "unlock-controllers"
    val substitutionControllersKey = "substitution-controllers"
    val vestingControllersKey = "vesting-controllers"

    def parseFields(memo: String): Either[String, Fields] =
      memo
        .stripPrefix(memoPrefix)
        .split('&')
        .toList
        .traverse(field =>
          field.split('=') match {
            case Array(k, v) => Right((k, v))
            case _ => Left(s"Expected `key=value`, got: $field")
          }
        )
        .map(_.toMap)

    private def parseRequiredField[A](fields: Fields, key: String)(
        parse: String => Either[String, A]
    ): Either[String, A] =
      fields.get(key) match {
        case Some(s) => parse(s)
        case None => Left(s"Missing required memo key: '$key'")
      }

    private def parseRequiredLongField[A](fields: Fields, key: String)(
        map: Long => A
    ): Either[String, A] =
      parseRequiredField(fields, key)(s =>
        s.toLongOption match {
          case Some(l) => Right(map(l))
          case None => Left(s"Failed to parse memo key '$key' as Long: '$s'")
        }
      )

    private def parseGovernanceLockKind(fields: Fields): Either[String, gl.GovernanceLockKind] =
      for {
        subject <- parseRequiredField(fields, lockSubjectKey)(Right(_))
        kind <- parseRequiredField(fields, lockKindKey) {
          case LockKind.SuperValidator.kind =>
            Right(new gl.governancelockkind.GLK_SuperValidatorRightsOwner(subject))
          case LockKind.FeaturedApp.kind =>
            Right(new gl.governancelockkind.GLK_FeaturedApp(subject))
          case LockKind.ProvisionalFeaturedApp.kind =>
            Right(new gl.governancelockkind.GLK_ProvisionalFeaturedApp(subject))
          case kind => Left(s"Unknown lock kind: '$kind'")
        }
      } yield kind

    private def parseControllersSpecification(
        fields: Fields,
        key: String,
    ): Either[String, Option[gl.controllers.ControllersSpecification]] =
      fields.get(key) match {
        case Some(s) =>
          decode[List[List[String]]](s).bimap(
            error => show"Error in memo key '$key': $error",
            controllers =>
              Some(
                new gl.controllers.ControllersSpecification(
                  controllers
                    .map(_.distinct.sorted)
                    .distinct
                    .sorted(Ordering.Implicits.seqOrdering[List, String])
                    .map(_.asJava)
                    .asJava
                )
              ),
          )
        case None => Right(None)
      }

    private def parseGovernanceLockControllers(
        fields: Fields
    ): Either[String, Option[gl.controllers.GovernanceLockControllers]] =
      for {
        unlockControllers <- parseControllersSpecification(fields, unlockControllersKey)
        substitutionControllers <- parseControllersSpecification(fields, substitutionControllersKey)
        vestingControllers <- parseControllersSpecification(fields, vestingControllersKey)
      } yield (unlockControllers, substitutionControllers, vestingControllers) match {
        case (None, None, None) => None
        case _ =>
          Some(
            new gl.controllers.GovernanceLockControllers(
              unlockControllers.toJava,
              substitutionControllers.toJava,
              vestingControllers.toJava,
            )
          )
      }

    private def parseFullLockSpecification(
        fields: Fields
    ): Either[String, gl.FullLockSpecification] =
      for {
        kind <- parseGovernanceLockKind(fields)
        controllers <- parseGovernanceLockControllers(fields)
      } yield new gl.FullLockSpecification(kind, controllers.toJava)

    private def parseVestingPeriod(fields: Fields): Either[String, RelTime] =
      parseRequiredLongField(fields, vestingDurationKey)(new RelTime(_))

    private def parseVestingEndTime(fields: Fields): Either[String, Instant] =
      parseRequiredLongField(fields, vestingEndTimeKey)(Instant.EPOCH.plus(_, ChronoUnit.MICROS))

    private def parseVestingLockSpecification(
        fields: Fields
    ): Either[String, gl.VestingLockSpecification] =
      for {
        kind <- parseGovernanceLockKind(fields)
        vestingPeriod <- parseVestingPeriod(fields)
        vestingEndTime <- parseVestingEndTime(fields)
        vestingControllers <- parseControllersSpecification(fields, vestingControllersKey)
      } yield new gl.VestingLockSpecification(
        kind,
        vestingPeriod,
        vestingEndTime,
        vestingControllers.toJava,
      )

    private def parseGovernanceLockSpecification(
        fields: Fields
    ): Either[String, gl.GovernanceLockSpecification] =
      fields.getOrElse(lockStatusKey, LockStatus.Locked.status) match {
        case LockStatus.Locked.status =>
          parseFullLockSpecification(fields)
            .map(new gl.governancelockspecification.GLS_FullLock(_))
        case LockStatus.Vesting.status =>
          parseVestingLockSpecification(fields)
            .map(new gl.governancelockspecification.GLS_VestingLock(_))
        case status => Left(s"Unknown lock status: '$status'")
      }

    private def parseUnlockTarget(
        dso: PartyId,
        sender: PartyId,
        fields: Fields,
    ): Either[String, gl.GovernanceLockKey] =
      for {
        specification <- parseFullLockSpecification(fields)
        owner = fields.getOrElse(targetLockOwnerKey, sender.toProtoPrimitive)
      } yield new gl.GovernanceLockKey(
        dso.toProtoPrimitive,
        owner,
        new gl.governancelockspecification.GLS_FullLock(specification),
      )

    private def parseTransferTarget(
        dso: PartyId,
        sender: PartyId,
        fields: Fields,
    ): Either[String, gl.GovernanceLockKey] =
      for {
        specification <- parseGovernanceLockSpecification(fields)
        owner = fields.getOrElse(targetLockOwnerKey, sender.toProtoPrimitive)
      } yield new gl.GovernanceLockKey(dso.toProtoPrimitive, owner, specification)

    private def parseWithdrawTarget(
        dso: PartyId,
        sender: PartyId,
        fields: Fields,
    ): Either[String, gl.GovernanceLockKey] =
      for {
        specification <- parseVestingLockSpecification(fields)
        owner = fields.getOrElse(targetLockOwnerKey, sender.toProtoPrimitive)
      } yield new gl.GovernanceLockKey(
        dso.toProtoPrimitive,
        owner,
        new gl.governancelockspecification.GLS_VestingLock(specification),
      )

    private def parseSubstitutionTarget(
        dso: PartyId,
        fields: Fields,
    ): Either[String, gl.GovernanceLockKey] =
      for {
        specification <- parseGovernanceLockSpecification(fields)
        owner <- parseRequiredField(fields, targetLockOwnerKey)(Right(_))
      } yield new gl.GovernanceLockKey(dso.toProtoPrimitive, owner, specification)

    def parseTarget(
        dso: PartyId,
        sender: PartyId,
        memo: String,
    ): Either[String, Option[gl.GovernanceLockKey]] =
      for {
        fields <- parseFields(memo)
        result <- parseRequiredField(fields, requestKey) {
          case `createLockRequest` => Right(Option.empty[gl.GovernanceLockKey])
          case `unlockRequest` => parseUnlockTarget(dso, sender, fields).map(Some(_))
          case `transferRequest` => parseTransferTarget(dso, sender, fields).map(Some(_))
          case `withdrawRequest` => parseWithdrawTarget(dso, sender, fields).map(Some(_))
          case `substituteRequest` => parseSubstitutionTarget(dso, fields).map(Some(_))
          case request => Left(s"Unknown request type: '$request'")
        }
      } yield result
  }
}
