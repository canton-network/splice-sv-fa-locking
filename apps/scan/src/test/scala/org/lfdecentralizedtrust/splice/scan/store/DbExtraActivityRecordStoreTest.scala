// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.scan.store

import com.digitalasset.canton.HasExecutionContext
import com.digitalasset.canton.lifecycle.FutureUnlessShutdown
import com.digitalasset.canton.resource.DbStorage
import com.digitalasset.canton.tracing.TraceContext
import org.lfdecentralizedtrust.splice.scan.store.db.DbExtraActivityRecordStore
import org.lfdecentralizedtrust.splice.scan.store.db.DbExtraActivityRecordStore.*
import org.lfdecentralizedtrust.splice.store.StoreTestBase
import org.lfdecentralizedtrust.splice.store.db.SplicePostgresTest

import scala.concurrent.Future

class DbExtraActivityRecordStoreTest
    extends StoreTestBase
    with HasExecutionContext
    with SplicePostgresTest {

  "DbExtraActivityRecordStore" should {

    "round-trip extra activity records through insert + read" in {
      for {
        store <- newStore()
        records = Seq(
          mkRecord(roundNumber = 10L, recordTime = 100L, appProvider = "app1", appWeight = 100L),
          mkRecord(
            roundNumber = 10L,
            recordTime = 101L,
            appProvider = "app2",
            appWeight = 50L,
            assetProvider = Some("asset1"),
            assetWeight = Some(25L),
          ),
          mkRecord(roundNumber = 11L, recordTime = 200L, appProvider = "app1", appWeight = 75L),
        )
        _ <- store.insertExtraActivityRecordsForTesting(records)
        loaded <- store.getRecordsForTesting()
      } yield {
        loaded shouldBe records
      }
    }

    "insert nothing on an empty batch" in {
      for {
        store <- newStore()
        _ <- store.insertExtraActivityRecordsForTesting(Seq.empty)
        loaded <- store.getRecordsForTesting()
      } yield {
        loaded shouldBe empty
      }
    }
  }

  private def mkRecord(
      roundNumber: Long,
      recordTime: Long,
      appProvider: String,
      appWeight: Long,
      assetProvider: Option[String] = None,
      assetWeight: Option[Long] = None,
  ): ExtraActivityRecordT =
    ExtraActivityRecordT(
      roundNumber = roundNumber,
      recordTime = recordTime,
      appProviderParties = Seq(appProvider),
      appActivityWeights = Seq(appWeight),
      assetActivityProviderParties = assetProvider.toList,
      assetActivityWeights = assetWeight.toList,
    )

  private val storeCounter = new java.util.concurrent.atomic.AtomicLong(1)

  private def newStore(): Future[DbExtraActivityRecordStore] = {
    val n = storeCounter.getAndIncrement()
    val participantId = mkParticipantId(s"extra-activity-test-$n")
    ScanExtraActivityRecordStore(dsoParty, participantId, storage, loggerFactory)
  }

  override protected def cleanDb(
      storage: DbStorage
  )(implicit traceContext: TraceContext): FutureUnlessShutdown[?] =
    resetAllAppTables(storage)
}
