// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.sv.automation.delegatebased

import org.lfdecentralizedtrust.splice.sv.automation.unavailableparties.UnavailablePartiesGuard
import com.digitalasset.canton.tracing.TraceContext
import io.opentelemetry.api.trace.Tracer
import org.apache.pekko.stream.Materializer
import org.lfdecentralizedtrust.splice.automation.*
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight
import org.lfdecentralizedtrust.splice.environment.PackageIdResolver
import org.lfdecentralizedtrust.splice.store.AppStoreWithIngestion.SpliceLedgerConnectionPriority
import org.lfdecentralizedtrust.splice.store.UnavailablePartiesStore
import org.lfdecentralizedtrust.splice.sv.config.SvAppBackendConfig
import org.lfdecentralizedtrust.splice.sv.store.FeaturedAppUnderlockConfig
import org.lfdecentralizedtrust.splice.sv.util.ContractStakeholders

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

import FeaturedAppPermanentUnderlockTrigger.{Task, getStakeholders}

/** Archives suspended `FeaturedAppRight`s whose `underlockPermanentAt` deadline has passed. */
class FeaturedAppPermanentUnderlockTrigger(
    override protected val svConfig: SvAppBackendConfig,
    override protected val context: TriggerContext,
    override protected val svTaskContext: SvTaskBasedTrigger.Context,
    override protected val unavailablePartiesStore: UnavailablePartiesStore,
)(implicit
    override val ec: ExecutionContext,
    mat: Materializer,
    tracer: Tracer,
) extends BatchedMultiDomainExpiredContractTrigger.Template[
      FeaturedAppRight.ContractId,
      FeaturedAppRight,
    ](
      svTaskContext.dsoStore.multiDomainAcsStore,
      svConfig.delegatelessAutomationFeaturedAppPermanentUnderlockBatchSize,
      svTaskContext.dsoStore.listFeaturedAppRightsPastUnderlockDeadline(
        Some(unavailablePartiesStore)
      ),
      FeaturedAppRight.COMPANION,
      svTaskContext.vettingLookupService,
      PackageIdResolver.Package.SpliceAmulet,
      getStakeholders,
    )
    with SvTaskBasedTrigger[Task]
    with UnavailablePartiesGuard {
  private val store = svTaskContext.dsoStore

  override def completeTaskAsDsoDelegate(task: Task, controller: String)(implicit
      tc: TraceContext
  ): Future[TaskOutcome] =
    completeUnlessAmuletVersionIgnored(
      task.work.vettedVersion.toString,
      task.work.stakeholders,
      ignoreUnresponsiveParties = true,
    )(archiveRights(task, controller))

  /** `DsoRules_EnforceFeaturedAppPermanentUnderlock` fails while enforcement is disabled, so we skip
    * the batch then. `FeaturedAppUnderlockStatusUpdateTrigger` lifts the suspensions in that case.
    */
  private def archiveRights(task: Task, controller: String)(implicit
      tc: TraceContext
  ): Future[TaskOutcome] = {
    val rights = task.work.expiredContracts
    for {
      featuredAppUnderlocking <- svTaskContext.packageVersionSupport.supportsFeaturedAppUnderlock(
        Seq(store.key.svParty, store.key.dsoParty),
        context.clock.now,
      )
      dsoRules <- store.getDsoRules()
      enforcementEnabled = FeaturedAppUnderlockConfig
        .fromDsoRulesConfig(dsoRules.payload.config)
        .enforcementEnabled
      outcome <-
        if (!featuredAppUnderlocking.supported || !enforcementEnabled) Future.successful(TaskNoop)
        else
          svTaskContext
            .connection(SpliceLedgerConnectionPriority.Low)
            .submit(
              Seq(store.key.svParty),
              Seq(store.key.dsoParty),
              update = rights.flatMap(right =>
                dsoRules
                  .exercise(
                    _.exerciseDsoRules_EnforceFeaturedAppPermanentUnderlock(
                      right.contractId,
                      controller,
                    )
                  )
                  .update
                  .commands()
                  .asScala
                  .toSeq
              ),
            )
            .noDedup
            .withSynchronizerId(dsoRules.domain)
            .yieldUnit()
            .map(_ =>
              TaskSuccess(s"archived ${rights.size} featured app rights past their underlock deadline")
            )
    } yield outcome
  }
}

object FeaturedAppPermanentUnderlockTrigger extends ContractStakeholders[FeaturedAppRight] {
  type Task =
    ScheduledTaskTrigger.ReadyTask[
      BatchedMultiDomainExpiredContractTrigger.Batch[FeaturedAppRight.ContractId, FeaturedAppRight]
    ]

  override def informees(payload: FeaturedAppRight): Seq[String] = Seq(payload.provider)

  override def dso(payload: FeaturedAppRight): String = payload.dso
}
