// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.scan.store.db

import com.digitalasset.canton.config.ProcessingTimeout
import com.digitalasset.canton.lifecycle.*
import com.digitalasset.canton.logging.{NamedLoggerFactory, NamedLogging}
import com.digitalasset.canton.resource.DbStorage
import com.digitalasset.canton.resource.DbStorage.Implicits.BuilderChain.*
import com.digitalasset.canton.tracing.TraceContext
import com.google.common.annotations.VisibleForTesting
import io.grpc.Status
import org.lfdecentralizedtrust.splice.scan.store.ActivityRecordMetaStore
import org.lfdecentralizedtrust.splice.scan.store.ActivityRecordMetaStore.RoundIngestionStatus
import org.lfdecentralizedtrust.splice.store.UpdateHistory
import org.lfdecentralizedtrust.splice.util.FutureUnlessShutdownUtil.futureUnlessShutdownToFuture
import slick.dbio.{DBIO, DBIOAction, Effect, NoStream}
import slick.jdbc.{GetResult, PostgresProfile}
import slick.jdbc.canton.SQLActionBuilder
import slick.jdbc.canton.ActionBasedSQLInterpolation.Implicits.actionBasedSQLInterpolationCanton

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.{ExecutionContext, Future}

object DbActivityRecordMetaStore {

  /** Code + user ingestion-version pair for the app side only. App meta
    * rows are keyed by `(history_id, code, user)`. The extra side keeps
    * one row per `store_id` and does not version.
    */
  final case class IngestionVersions(code: Int, user: Int)

  /** Scope descriptor used to parameterize shared meta-SQL helpers over
    * each side's scope column. App side uses `history_id` and additional
    * `(code_version, user_version)` key columns; extra side uses
    * `store_id` only.
    *
    * `filter` = `scope-col = $value` only.
    * `metaFilter` = full WHERE for a single meta row
    * (app: `scope.filter AND code_version = ... AND user_version = ...`;
    *  extra: `scope.filter`).
    */
  private final case class SideScope(
      column: String,
      filter: SQLActionBuilder,
      valueSql: SQLActionBuilder,
      metaFilter: SQLActionBuilder,
  )

  /** Metadata row for app-side activity ingestion (TBAR), read from and
    * written to `app_activity_record_meta`. Per-table row types let the
    * two sides diverge in schema without a rename.
    */
  final case class AppActivityRecordMetaT(
      historyId: Long,
      codeVersion: Int,
      userVersion: Int,
      startedIngestingAt: Long,
      earliestIngestedRound: Long,
      lastArchivedRound: Option[Long],
  )

  /** Metadata row for extra-activity ingestion (CIP-0104 extension hook),
    * read from and written to `extra_app_activity_record_meta`. One row
    * per `store_id`; no ingestion-version tracking.
    */
  final case class ExtraActivityRecordMetaT(
      storeId: Int,
      startedIngestingAt: Long,
      earliestIngestedRound: Long,
      lastArchivedRound: Option[Long],
  )

  sealed trait MetaCheckResult
  case object InsertMeta extends MetaCheckResult
  case object Resume extends MetaCheckResult
  final case class DowngradeDetected(
      runningCode: Int,
      runningUser: Int,
      storedCode: Int,
      storedUser: Int,
  ) extends MetaCheckResult

  def checkMetaVersions(
      existing: Option[(Int, Int)],
      runningCode: Int,
      runningUser: Int,
  ): MetaCheckResult = existing match {
    case None => InsertMeta
    case Some((storedCode, storedUser)) =>
      if (runningCode < storedCode || runningUser < storedUser)
        DowngradeDetected(runningCode, runningUser, storedCode, storedUser)
      else if (runningCode > storedCode || runningUser > storedUser)
        InsertMeta
      else
        Resume
  }

  /** Four-case branch table for `ingestionStatusForRound`. Pure decision
    * over `(isFirstSv, earliestIngested, roundNumber)`:
    *   - meta present, round within ingested range → `CannotProvide`
    *     (we should have data; a peer likely does — delegate)
    *   - meta present, round beyond ingested boundary → `Undetermined`
    *     (ingestion catching up; retry)
    *   - meta absent on a late-joining Scan (`!isFirstSv`) → `CannotProvide`
    *   - meta absent on firstSV (startup race before the first insert) →
    *     `Undetermined`
    */
  def ingestionStatusFor(
      isFirstSv: Boolean,
      earliestIngested: Option[Long],
      roundNumber: Long,
  ): RoundIngestionStatus = earliestIngested match {
    case Some(earliest) if roundNumber <= earliest => RoundIngestionStatus.CannotProvide
    case Some(_) => RoundIngestionStatus.Undetermined
    case None if !isFirstSv => RoundIngestionStatus.CannotProvide
    case None => RoundIngestionStatus.Undetermined
  }
}

/** Implementation of `ActivityRecordMetaStore`, owning the meta surface for
  * both activity-record sides: the TBAR app side (`app_activity_record_meta`,
  * fed from verdict ingestion) and the CIP-0104 extension hook side
  * (`extra_app_activity_record_meta`, fed from
  * `ActivityRecorder_DsoRecordActivity` exercises). The trait methods
  * return a combined view across the two sides.
  *
  * The two concrete row stores (`DbAppActivityRecordStore`,
  * `DbExtraActivityRecordStore`) do not touch their meta tables; they
  * consume the per-side meta helpers here. Row-level case classes and
  * insert/read paths stay with the concrete stores.
  *
  * Combine semantics:
  *   - `earliestRoundWithCompleteAppActivity`: max of the two sides —
  *     completeness from genesis requires BOTH to have reached the round.
  *     Returns None if either side is None.
  *   - `latestRoundWithCompleteAppActivity`: min of the two — the merged
  *     frontier is bounded by whichever side is slower. Returns None if
  *     either side is None.
  *   - `ingestionStatusForRound`: if either side reports `CannotProvide`,
  *     the result is `CannotProvide` (caller should delegate to BFT).
  *     Otherwise `Undetermined` (either or both sides still catching up).
  *   - `startedIngestingAt`: the earlier of the two start times when both
  *     are available; the single non-None value otherwise.
  */
class DbActivityRecordMetaStore(
    storage: DbStorage,
    updateHistory: UpdateHistory,
    extraActivityRecordStore: DbExtraActivityRecordStore,
    val ingestionVersions: DbActivityRecordMetaStore.IngestionVersions,
    isFirstSv: Boolean,
    override protected val loggerFactory: NamedLoggerFactory,
)(implicit ec: ExecutionContext)
    extends ActivityRecordMetaStore
    with NamedLogging
    with org.lfdecentralizedtrust.splice.store.db.AcsJdbcTypes
    with org.lfdecentralizedtrust.splice.store.db.AcsQueries
    with FlagCloseable
    with HasCloseContext {

  val profile: slick.jdbc.JdbcProfile = PostgresProfile

  override protected def timeouts = new ProcessingTimeout

  import DbActivityRecordMetaStore.*

  private[store] object Tables {
    val appActivityRecordMeta = "app_activity_record_meta"
    val extraActivityRecordMeta = "extra_app_activity_record_meta"
  }

  private def historyId = updateHistory.historyId
  private def extraStoreId = extraActivityRecordStore.storeId

  private def appScope = {
    val codeVersion = ingestionVersions.code
    val userVersion = ingestionVersions.user
    SideScope(
      column = "history_id",
      filter = sql"history_id = $historyId",
      valueSql = sql"$historyId",
      metaFilter = sql"history_id = $historyId" ++
        sql" and activity_ingestion_code_version = $codeVersion" ++
        sql" and activity_ingestion_user_version = $userVersion",
    )
  }
  private def extraScope =
    SideScope(
      column = "store_id",
      filter = sql"store_id = $extraStoreId",
      valueSql = sql"$extraStoreId",
      metaFilter = sql"store_id = $extraStoreId",
    )

  private def scopeFor(table: String): SideScope =
    if (table == Tables.appActivityRecordMeta) appScope else extraScope

  private implicit val getResultAppActivityRecordMeta: GetResult[AppActivityRecordMetaT] =
    GetResult { prs =>
      AppActivityRecordMetaT(
        historyId = prs.<<[Long],
        codeVersion = prs.<<[Int],
        userVersion = prs.<<[Int],
        startedIngestingAt = prs.<<[Long],
        earliestIngestedRound = prs.<<[Long],
        lastArchivedRound = prs.<<[Option[Long]],
      )
    }

  private implicit val getResultExtraActivityRecordMeta: GetResult[ExtraActivityRecordMetaT] =
    GetResult { prs =>
      ExtraActivityRecordMetaT(
        storeId = prs.<<[Int],
        startedIngestingAt = prs.<<[Long],
        earliestIngestedRound = prs.<<[Long],
        lastArchivedRound = prs.<<[Option[Long]],
      )
    }

  // ============ Public combine overrides ============

  override def startedIngestingAt(implicit tc: TraceContext): Future[Option[Long]] =
    appStartedIngestingAt
      .zip(
        cachedStartedIngestingAt(
          extraStartedIngestingAtRef,
          lookupExtraActivityRecordMeta().map(_.map(_.startedIngestingAt)),
        )
      )
      .map {
        case (Some(a), Some(e)) => Some(math.min(a, e))
        case (someA, None) => someA
        case (None, someE) => someE
      }

  override def earliestRoundWithCompleteAppActivity()(implicit
      tc: TraceContext
  ): Future[Option[Long]] =
    earliestCompleteRoundOf(Tables.appActivityRecordMeta)
      .zip(earliestCompleteRoundOf(Tables.extraActivityRecordMeta))
      .map {
        case (Some(a), Some(e)) => Some(math.max(a, e))
        case _ => None
      }

  override def latestRoundWithCompleteAppActivity()(implicit
      tc: TraceContext
  ): Future[Option[Long]] =
    latestCompleteRoundOf(Tables.appActivityRecordMeta)
      .zip(latestCompleteRoundOf(Tables.extraActivityRecordMeta))
      .map {
        case (Some(a), Some(e)) => Some(math.min(a, e))
        case _ => None
      }

  override def ingestionStatusForRound(roundNumber: Long)(implicit
      tc: TraceContext
  ): Future[RoundIngestionStatus] =
    metaIngestionStatus(earliestIngestedRoundOf(Tables.appActivityRecordMeta), roundNumber)
      .zip(
        metaIngestionStatus(earliestIngestedRoundOf(Tables.extraActivityRecordMeta), roundNumber)
      )
      .map {
        case (RoundIngestionStatus.CannotProvide, _) => RoundIngestionStatus.CannotProvide
        case (_, RoundIngestionStatus.CannotProvide) => RoundIngestionStatus.CannotProvide
        case _ => RoundIngestionStatus.Undetermined
      }

  // ============ Per-side meta readers ============

  private[store] def earliestIngestedRoundOf(
      table: String
  )(implicit tc: TraceContext): Future[Option[Long]] = {
    val scope = scopeFor(table)
    runQuerySingle(
      (sql"""select earliest_ingested_round from #$table where """ ++ scope.metaFilter)
        .as[Option[Long]]
        .headOption
        .map(_.flatten),
      s"$table.earliestIngestedRound",
    )
  }

  /** The first ingested round may be partial, so the earliest complete
    * round is `earliest_ingested_round + 1`. Returns only once that round
    * has been archived.
    */
  private[store] def earliestCompleteRoundOf(
      table: String
  )(implicit tc: TraceContext): Future[Option[Long]] = {
    val scope = scopeFor(table)
    runQuerySingle(
      (sql"""select earliest_ingested_round + 1
             from #$table
             where """ ++ scope.metaFilter ++
        sql" and last_archived_round >= earliest_ingested_round + 1")
        .as[Option[Long]]
        .headOption
        .map(_.flatten),
      s"$table.earliestRoundWithCompleteAppActivity",
    )
  }

  /** Latest round with complete activity (= `last_archived_round`). */
  private[store] def latestCompleteRoundOf(
      table: String
  )(implicit tc: TraceContext): Future[Option[Long]] = {
    val scope = scopeFor(table)
    runQuerySingle(
      (sql"""select last_archived_round from #$table where """ ++ scope.metaFilter)
        .as[Option[Long]]
        .headOption
        .map(_.flatten),
      s"$table.latestRoundWithCompleteAppActivity",
    )
  }

  // ============ App-side meta ============

  def appStartedIngestingAt(implicit tc: TraceContext): Future[Option[Long]] =
    cachedStartedIngestingAt(
      appStartedIngestingAtRef,
      lookupAppActivityRecordMeta(ingestionVersions.code, ingestionVersions.user)
        .map(_.map(_.startedIngestingAt)),
    )

  def ensureAppMetaDBIO(
      ingestionStart: (Long, Long),
      lastArchivedRoundO: Option[Long] = None,
      exitOnDowngrade: Boolean = true,
  )(implicit tc: TraceContext): DBIO[MetaCheckResult] =
    ensureAppMetaDBIOImpl(ingestionStart, lastArchivedRoundO, exitOnDowngrade)

  def updateAppLastArchivedRoundDBIO(round: Long) =
    updateLastArchivedRoundDBIO(Tables.appActivityRecordMeta)(round)

  @VisibleForTesting
  def insertAppActivityRecordMetaForTesting(
      codeVersion: Int,
      userVersion: Int,
      startedIngestingAt: Long,
      earliestIngestedRound: Long,
      lastArchivedRound: Option[Long],
  )(implicit tc: TraceContext): Future[Unit] =
    futureUnlessShutdownToFuture(
      storage.update_(
        insertAppMetaDBIO(
          codeVersion,
          userVersion,
          startedIngestingAt,
          earliestIngestedRound,
          lastArchivedRound,
        ),
        "appActivity.insertAppActivityRecordMeta",
      )
    )

  private[store] def lookupAppActivityRecordMeta(codeVersion: Int, userVersion: Int)(implicit
      tc: TraceContext
  ): Future[Option[AppActivityRecordMetaT]] =
    runQuerySingle(
      sql"""select history_id, activity_ingestion_code_version,
                   activity_ingestion_user_version, started_ingesting_at,
                   earliest_ingested_round, last_archived_round
            from #${Tables.appActivityRecordMeta}
            where history_id = $historyId
              and activity_ingestion_code_version = $codeVersion
              and activity_ingestion_user_version = $userVersion
      """.as[AppActivityRecordMetaT].headOption,
      "appActivity.lookupAppActivityRecordMeta",
    )

  /** Assert that all activity records for roundNumber have been ingested
    * on the app side. The round must be greater than earliest_ingested_round
    * and less than or equal to last_archived_round.
    */
  private[store] def appAssertCompleteActivity(roundNumber: Long)(implicit
      tc: TraceContext
  ): Future[Unit] =
    lookupAppActivityRecordMeta(ingestionVersions.code, ingestionVersions.user).map { metaO =>
      val earliestO = metaO.map(_.earliestIngestedRound)
      val lastArchivedO = metaO.flatMap(_.lastArchivedRound)
      (earliestO, lastArchivedO) match {
        case (Some(earliest), Some(lastArchived))
            if roundNumber > earliest && roundNumber <= lastArchived =>
          ()
        case _ =>
          throw Status.FAILED_PRECONDITION
            .withDescription(
              s"Incomplete app activity for round $roundNumber: we require " +
                s"earliest_ingested_round < round <= last_archived_round, but " +
                s"earliest_ingested_round=$earliestO, last_archived_round=$lastArchivedO"
            )
            .asRuntimeException()
      }
    }

  private val appStartedIngestingAtRef = new AtomicReference[Option[Long]](None)
  private val appMetaChecked = new AtomicBoolean(false)

  // ============ Extra-side meta ============

  /** Idempotent insert-if-absent for the extra-side meta row. One row
    * per `store_id`; no version check, so any `MetaCheckResult` other
    * than `Resume` / `InsertMeta` is not reachable here.
    */
  def ensureExtraMetaDBIO(
      ingestionStart: (Long, Long),
      lastArchivedRoundO: Option[Long] = None,
  )(implicit tc: TraceContext): DBIO[MetaCheckResult] = {
    val (firstRecordTimeMicros, earliestRound) = ingestionStart
    if (extraMetaChecked.get()) DBIO.successful(Resume)
    else {
      insertExtraMetaDBIO(firstRecordTimeMicros, earliestRound, lastArchivedRoundO)
        .map { inserted =>
          extraMetaChecked.set(true)
          if (inserted > 0) {
            logger.info("Extra activity ingestion inserted new meta row.")
            InsertMeta: MetaCheckResult
          } else {
            logger.info("Extra activity ingestion resumed with existing meta row.")
            Resume: MetaCheckResult
          }
        }
    }
  }

  def updateExtraLastArchivedRoundDBIO(round: Long) =
    updateLastArchivedRoundDBIO(Tables.extraActivityRecordMeta)(round)

  @VisibleForTesting
  def insertExtraActivityRecordMetaForTesting(
      startedIngestingAt: Long,
      earliestIngestedRound: Long,
      lastArchivedRound: Option[Long],
  )(implicit tc: TraceContext): Future[Unit] =
    futureUnlessShutdownToFuture(
      storage.update_(
        insertExtraMetaDBIO(startedIngestingAt, earliestIngestedRound, lastArchivedRound),
        "extraActivity.insertExtraActivityRecordMeta",
      )
    )

  @VisibleForTesting
  def bumpExtraLastArchivedRoundForTesting(round: Long)(implicit
      tc: TraceContext
  ): Future[Unit] =
    futureUnlessShutdownToFuture(
      storage.update_(
        updateExtraLastArchivedRoundDBIO(round),
        "extraActivity.updateLastArchivedRound",
      )
    )

  private[store] def lookupExtraActivityRecordMeta()(implicit
      tc: TraceContext
  ): Future[Option[ExtraActivityRecordMetaT]] =
    runQuerySingle(
      sql"""select store_id, started_ingesting_at,
                   earliest_ingested_round, last_archived_round
            from #${Tables.extraActivityRecordMeta}
            where store_id = $extraStoreId
      """.as[ExtraActivityRecordMetaT].headOption,
      "extraActivity.lookupExtraActivityRecordMeta",
    )

  private val extraStartedIngestingAtRef = new AtomicReference[Option[Long]](None)
  private val extraMetaChecked = new AtomicBoolean(false)

  // ============ Shared helpers ============

  /** Cache-when-non-None helper for `started_ingesting_at`: once `lookup`
    * observes a Some, the cached value is returned without further lookups
    * (the column is monotonic-set-once).
    */
  private def cachedStartedIngestingAt(
      cache: AtomicReference[Option[Long]],
      lookup: => Future[Option[Long]],
  ): Future[Option[Long]] =
    cache.get() match {
      case some @ Some(_) => Future.successful(some)
      case None =>
        lookup.map { tsO =>
          tsO.foreach(ts => cache.compareAndSet(None, Some(ts)))
          tsO
        }
    }

  private def metaIngestionStatus(
      earliestF: Future[Option[Long]],
      roundNumber: Long,
  ): Future[RoundIngestionStatus] =
    earliestF.map(ingestionStatusFor(isFirstSv, _, roundNumber))

  /** App-side meta insert (keyed by `(history_id, code_version, user_version)`,
    * idempotent via `ON CONFLICT DO NOTHING`).
    */
  private def insertAppMetaDBIO(
      codeVersion: Int,
      userVersion: Int,
      startedIngestingAt: Long,
      earliestIngestedRound: Long,
      lastArchivedRound: Option[Long],
  ) =
    sql"""insert into #${Tables.appActivityRecordMeta}
            (history_id, activity_ingestion_code_version,
             activity_ingestion_user_version, started_ingesting_at,
             earliest_ingested_round, last_archived_round)
          values ($historyId, $codeVersion, $userVersion, $startedIngestingAt,
                  $earliestIngestedRound, $lastArchivedRound)
          ON CONFLICT DO NOTHING
    """.asUpdate

  /** Extra-side meta insert (keyed by `store_id`, idempotent via
    * `ON CONFLICT DO NOTHING`). Returns the row-count — 1 on insert, 0
    * on existing row.
    */
  private def insertExtraMetaDBIO(
      startedIngestingAt: Long,
      earliestIngestedRound: Long,
      lastArchivedRound: Option[Long],
  ) =
    sql"""insert into #${Tables.extraActivityRecordMeta}
            (store_id, started_ingesting_at,
             earliest_ingested_round, last_archived_round)
          values ($extraStoreId, $startedIngestingAt,
                  $earliestIngestedRound, $lastArchivedRound)
          ON CONFLICT DO NOTHING
    """.asUpdate

  /** Update `last_archived_round` on a meta row. */
  private def updateLastArchivedRoundDBIO(table: String)(round: Long) = {
    val scope = scopeFor(table)
    (sql"""update #$table
          set last_archived_round = $round,
              last_updated_at = now()
          where """ ++ scope.metaFilter).asUpdate
  }

  /** App-side ensure: version-check + insert; log
    * `Resume` / `InsertMeta` / `DowngradeDetected`; on downgrade,
    * `sys.exit(1)` when `exitOnDowngrade = true`, else return the result.
    * Short-circuits once `appMetaChecked` observes a successful check.
    */
  private def ensureAppMetaDBIOImpl(
      ingestionStart: (Long, Long),
      lastArchivedRoundO: Option[Long],
      exitOnDowngrade: Boolean,
  )(implicit tc: TraceContext): DBIO[MetaCheckResult] = {
    val codeVersion = ingestionVersions.code
    val userVersion = ingestionVersions.user
    val (firstRecordTimeMicros, earliestRound) = ingestionStart
    val logPrefix = "App activity ingestion"
    if (appMetaChecked.get()) DBIO.successful(Resume)
    else {
      for {
        maxVersions <-
          sql"""select max(activity_ingestion_code_version),
                       max(activity_ingestion_user_version)
                from #${Tables.appActivityRecordMeta}
                where history_id = $historyId
              """
            .as[(Option[Int], Option[Int])]
            .headOption
            .map(_.flatMap {
              case (Some(c), Some(u)) => Some((c, u))
              case _ => None
            })
        result <- checkMetaVersions(maxVersions, codeVersion, userVersion) match {
          case InsertMeta =>
            insertAppMetaDBIO(
              codeVersion,
              userVersion,
              firstRecordTimeMicros,
              earliestRound,
              lastArchivedRoundO,
            ).map { _ =>
              logger.info(
                s"$logPrefix inserted new meta row for " +
                  s"codeVersion=$codeVersion, userVersion=$userVersion."
              )
              appMetaChecked.set(true)
              InsertMeta: MetaCheckResult
            }
          case Resume =>
            logger.info(
              s"$logPrefix resumed with existing meta row for " +
                s"codeVersion=$codeVersion, userVersion=$userVersion."
            )
            DBIO.successful {
              appMetaChecked.set(true)
              Resume: MetaCheckResult
            }
          case d: DowngradeDetected =>
            logger.error(
              s"$logPrefix version downgrade detected: " +
                s"running=(${d.runningCode},${d.runningUser}), stored=(${d.storedCode},${d.storedUser}). " +
                s"Make sure you did not accidentally remove or downgrade the 'activity-ingestion-user-version' field" +
                s"in the scan app config. Shutting down to prevent data corruption."
            )
            if (exitOnDowngrade) {
              sys.exit(1)
            } else {
              DBIO.successful(d: MetaCheckResult)
            }
        }
      } yield result
    }
  }

  private def runQuerySingle[T](
      action: DBIOAction[Option[T], NoStream, Effect.Read],
      operationName: String,
  )(implicit tc: TraceContext): Future[Option[T]] =
    futureUnlessShutdownToFuture(storage.querySingle(action, operationName).value)
}
