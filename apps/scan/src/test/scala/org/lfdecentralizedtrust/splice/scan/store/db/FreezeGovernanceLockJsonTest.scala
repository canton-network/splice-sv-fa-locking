// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.scan.store.db

import org.lfdecentralizedtrust.splice.codegen.java.da.time.types.RelTime
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock as gl
import com.digitalasset.canton.BaseTest
import io.circe.Json
import io.circe.syntax.*
import java.time.Instant
import java.util.Optional
import org.scalatest.wordspec.AnyWordSpec

// ATTENTION: if this test fails, make sure that
// `DbScanStore.listGovernanceLocksByKey` still finds locks stored by earlier
// releases before you update the expected strings.
//
// Scan looks up governance locks by key by comparing the stored
// `create_arguments->'specification'` with the JSON of a
// `GovernanceLockSpecification`, using jsonb equality. The stored value was
// written by the release that ingested the lock, so if the JSON of a
// specification changes between releases, the lookup stops finding older
// locks.
//
// This test pins the current JSON of every kind of specification, as a
// tripwire for such changes.
//
// Both sides of the comparison drop the fields with a `null` value, so a new
// optional field does not break the lookup: earlier releases stored values
// without the field, later releases store it as `"field": null`. A rename of
// a field or constructor, or a change of the encoding, still breaks it.
class FreezeGovernanceLockJsonTest extends AnyWordSpec with BaseTest {
  private val provider = "provider::1220aaaa"
  private val featuredApp = new gl.governancelockkind.GLK_FeaturedApp(provider)
  private val provisionalFeaturedApp =
    new gl.governancelockkind.GLK_ProvisionalFeaturedApp(provider)
  private val superValidator =
    new gl.governancelockkind.GLK_SuperValidatorRightsOwner("sv1")

  // Default vesting periods per lock kind, in microseconds.
  // 0 days
  private val provisionalFeaturedAppVestingPeriod = new RelTime(0L)
  // 60 days
  private val featuredAppVestingPeriod = new RelTime(5_184_000_000_000L)
  // 365.25 days (365 days and 6 hours)
  private val superValidatorVestingPeriod = new RelTime(31_557_600_000_000L)
  private val endTime = Instant.parse("2026-10-27T12:34:56.123456Z")

  private def fullLock(kind: gl.GovernanceLockKind): Json =
    DbScanStore.governanceLockSpecificationJson(
      new gl.governancelockspecification.GLS_FullLock(
        new gl.FullLockSpecification(kind, Optional.empty())
      )
    )

  private def vestingLock(
      kind: gl.GovernanceLockKind,
      vestingPeriod: RelTime,
  ): Json =
    DbScanStore.governanceLockSpecificationJson(
      new gl.governancelockspecification.GLS_VestingLock(
        new gl.VestingLockSpecification(kind, vestingPeriod, endTime, Optional.empty())
      )
    )

  "The JSON of a governance lock specification" should {
    "not change for a featured app full lock" in {
      fullLock(featuredApp) shouldBe Json.obj(
        "tag" := "GLS_FullLock",
        "value" := Json.obj(
          "kind" := Json.obj(
            "tag" := "GLK_FeaturedApp",
            "value" := Json.obj("provider" := provider),
          )
        ),
      )
    }

    "not change for a provisional featured app full lock" in {
      fullLock(provisionalFeaturedApp) shouldBe Json.obj(
        "tag" := "GLS_FullLock",
        "value" := Json.obj(
          "kind" := Json.obj(
            "tag" := "GLK_ProvisionalFeaturedApp",
            "value" := Json.obj("provider" := provider),
          )
        ),
      )
    }

    "not change for a super validator full lock" in {
      fullLock(superValidator) shouldBe Json.obj(
        "tag" := "GLS_FullLock",
        "value" := Json.obj(
          "kind" := Json.obj(
            "tag" := "GLK_SuperValidatorRightsOwner",
            "value" := Json.obj("svName" := "sv1"),
          )
        ),
      )
    }

    "not change for a featured app vesting lock" in {
      vestingLock(featuredApp, featuredAppVestingPeriod) shouldBe Json.obj(
        "tag" := "GLS_VestingLock",
        "value" := Json.obj(
          "kind" := Json.obj(
            "tag" := "GLK_FeaturedApp",
            "value" := Json.obj("provider" := provider),
          ),
          "vestingPeriod" := Json.obj("microseconds" := 5184000000000L),
          "endTime" := "2026-10-27T12:34:56.123456Z",
        ),
      )
    }

    "not change for a provisional featured app vesting lock" in {
      vestingLock(
        provisionalFeaturedApp,
        provisionalFeaturedAppVestingPeriod,
      ) shouldBe Json.obj(
        "tag" := "GLS_VestingLock",
        "value" := Json.obj(
          "kind" := Json.obj(
            "tag" := "GLK_ProvisionalFeaturedApp",
            "value" := Json.obj("provider" := provider),
          ),
          "vestingPeriod" := Json.obj("microseconds" := 0),
          "endTime" := "2026-10-27T12:34:56.123456Z",
        ),
      )
    }

    "not change for a super validator vesting lock" in {
      vestingLock(superValidator, superValidatorVestingPeriod) shouldBe Json.obj(
        "tag" := "GLS_VestingLock",
        "value" := Json.obj(
          "kind" := Json.obj(
            "tag" := "GLK_SuperValidatorRightsOwner",
            "value" := Json.obj("svName" := "sv1"),
          ),
          "vestingPeriod" := Json.obj("microseconds" := 31557600000000L),
          "endTime" := "2026-10-27T12:34:56.123456Z",
        ),
      )
    }
  }
}
