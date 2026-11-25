// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.util

import com.digitalasset.canton.topology.PartyId
import io.grpc.Status
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock as gl

import java.util.Optional
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

object GovernanceLockUtil {

  /** Receiver of all governance lock requests, mirroring `lockManagerParty` in `Splice.GovernanceLock`. */
  val lockManagerParty = PartyId.tryFromProtoPrimitive(
    "cip-127_lock-manager::1220000000000000000000000000000000000000000000000000000000000000abcd"
  )

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
}
