// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.util

import io.grpc.Status
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock as gl

object GovernanceLockUtil {
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

  def getVestingLockSpecification(lock: gl.GovernanceLock): Option[gl.VestingLockSpecification] =
    foldSpecification(lock.specification)(_ => None, Some(_))

  /** Parties observing a lock with this specification, mirroring `governanceLockObservers`
    * in `Splice.GovernanceLock`.
    */
  def specificationObservers(specification: gl.GovernanceLockSpecification): Seq[String] =
    getSpecificationKind(specification) match {
      case kind: gl.governancelockkind.GLK_FeaturedApp => Seq(kind.provider)
      case kind: gl.governancelockkind.GLK_ProvisionalFeaturedApp => Seq(kind.provider)
      case _ => Seq.empty
    }

  def governanceLockObservers(lock: gl.GovernanceLock): Seq[String] =
    specificationObservers(lock.specification)

  /** Encodes a lock as the `governance_lock_type` and `governance_lock_subject` index columns. */
  def specificationTypeAndSubject(
      specification: gl.GovernanceLockSpecification
  ): (String, String) = {
    val lockType = foldSpecification(specification)(_ => "f", _ => "v")
    val (kindTag, subject) = getSpecificationKind(specification) match {
      case kind: gl.governancelockkind.GLK_FeaturedApp => ("fa", kind.provider)
      case kind: gl.governancelockkind.GLK_ProvisionalFeaturedApp => ("pfa", kind.provider)
      case kind: gl.governancelockkind.GLK_SuperValidatorRightsOwner => ("sv", kind.svName)
      case kind =>
        throw Status.INTERNAL
          .withDescription(s"Unexpected GovernanceLockKind: $kind")
          .asRuntimeException()
    }
    (s"$lockType:$kindTag", subject)
  }

  def governanceLockTypeAndSubject(lock: gl.GovernanceLock): (String, String) =
    specificationTypeAndSubject(lock.specification)
}
