// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.scan.store.db

import org.lfdecentralizedtrust.splice.util.FutureUnlessShutdownUtil.futureUnlessShutdownToFuture
import com.digitalasset.canton.lifecycle.CloseContext
import com.digitalasset.canton.logging.{NamedLoggerFactory, NamedLogging}
import com.digitalasset.canton.resource.DbStorage
import com.digitalasset.canton.resource.DbStorage.Implicits.BuilderChain.*
import com.digitalasset.canton.tracing.TraceContext
import com.digitalasset.canton.lifecycle.*
import com.digitalasset.canton.config.ProcessingTimeout
import com.google.common.annotations.VisibleForTesting
import slick.jdbc.{GetResult, PostgresProfile}
import slick.jdbc.canton.ActionBasedSQLInterpolation.Implicits.actionBasedSQLInterpolationCanton
import slick.dbio.DBIO

import scala.concurrent.{ExecutionContext, Future}

object DbExtraActivityRecordStore {

  /** Extra activity record contributed by an
    * `ActivityRecorder_DsoRecordActivity` exercise (CIP-0104 amendment).
    * `store_id` is injected from the store descriptor on insert; the DB
    * PK is `(store_id, round_number, record_time)`.
    */
  final case class ExtraActivityRecordT(
      roundNumber: Long,
      recordTime: Long,
      appProviderParties: Seq[String],
      appActivityWeights: Seq[Long],
      assetActivityProviderParties: Seq[String] = Seq.empty,
      assetActivityWeights: Seq[Long] = Seq.empty,
  )

  def apply(
      storeDescriptor: org.lfdecentralizedtrust.splice.store.db.StoreDescriptor,
      storage: DbStorage,
      loggerFactory: NamedLoggerFactory,
  )(implicit
      ec: ExecutionContext,
      cc: CloseContext,
      tc: TraceContext,
  ): Future[DbExtraActivityRecordStore] =
    org.lfdecentralizedtrust.splice.store.db.StoreDescriptorStore
      .getStoreIdForDescriptor(storeDescriptor, storage)
      .map(storeId => new DbExtraActivityRecordStore(storage, storeId, loggerFactory))
}

/** Row store for extra-activity records contributed by the CIP-0104
  * amendment's `ActivityRecorder_DsoRecordActivity` choice, backed by
  * `extra_app_activity_record_store`.
  */
class DbExtraActivityRecordStore private (
    storage: DbStorage,
    val storeId: Int,
    override protected val loggerFactory: NamedLoggerFactory,
)(implicit
    ec: ExecutionContext
) extends NamedLogging
    with org.lfdecentralizedtrust.splice.store.db.AcsJdbcTypes
    with org.lfdecentralizedtrust.splice.store.db.AcsQueries
    with FlagCloseable
    with HasCloseContext {

  val profile: slick.jdbc.JdbcProfile = PostgresProfile

  override protected def timeouts = new ProcessingTimeout

  private[store] object Tables {
    val extraActivityRecords = "extra_app_activity_record_store"
  }

  type ExtraActivityRecordT = DbExtraActivityRecordStore.ExtraActivityRecordT

  private implicit val getResultExtraActivityRecord: GetResult[ExtraActivityRecordT] =
    GetResult { prs =>
      DbExtraActivityRecordStore.ExtraActivityRecordT(
        roundNumber = prs.<<[Long],
        recordTime = prs.<<[Long],
        appProviderParties = stringSeqGetResult(prs),
        appActivityWeights = longSeqGetResult(prs),
        assetActivityProviderParties = stringSeqGetResult(prs),
        assetActivityWeights = longSeqGetResult(prs),
      )
    }

  private def batchInsertExtraActivityRecords(items: Seq[ExtraActivityRecordT]) =
    if (items.isEmpty) {
      DBIO.successful(0)
    } else {
      val values = sqlCommaSeparated(
        items.map { row =>
          sql"""($storeId, ${row.roundNumber}, ${row.recordTime},
                ${row.appProviderParties}, ${row.appActivityWeights},
                ${row.assetActivityProviderParties}, ${row.assetActivityWeights})"""
        }
      )
      (sql"""
        insert into #${Tables.extraActivityRecords}(
          store_id, round_number, record_time,
          app_provider_parties, app_activity_weights,
          asset_activity_provider_parties, asset_activity_weights
        ) values """ ++ values ++ sql" ON CONFLICT DO NOTHING").asUpdate
    }

  @VisibleForTesting
  def insertExtraActivityRecordsForTesting(
      items: Seq[ExtraActivityRecordT]
  )(implicit tc: TraceContext): Future[Unit] =
    if (items.isEmpty) Future.unit
    else
      futureUnlessShutdownToFuture(
        storage.update_(
          batchInsertExtraActivityRecords(items),
          "extraActivity.insertExtraActivityRecords.batch",
        )
      )

  /** Read every row for this store's `store_id`, ordered by
    * `(round_number, record_time)`.
    */
  @VisibleForTesting
  def getRecordsForTesting()(implicit tc: TraceContext): Future[Seq[ExtraActivityRecordT]] =
    futureUnlessShutdownToFuture(
      storage.query(
        sql"""
          select round_number, record_time,
                 app_provider_parties, app_activity_weights,
                 asset_activity_provider_parties, asset_activity_weights
          from #${Tables.extraActivityRecords}
          where store_id = $storeId
          order by round_number, record_time
        """.as[ExtraActivityRecordT],
        "extraActivity.getRecordsForTesting",
      )
    )
}
