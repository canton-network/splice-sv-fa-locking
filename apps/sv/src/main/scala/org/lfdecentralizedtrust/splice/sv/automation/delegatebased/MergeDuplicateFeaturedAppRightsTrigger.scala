// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.sv.automation.delegatebased

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.logging.pretty.{Pretty, PrettyPrinting}
import com.digitalasset.canton.topology.PartyId
import com.digitalasset.canton.tracing.TraceContext
import com.digitalasset.canton.util.MonadUtil
import io.opentelemetry.api.trace.Tracer
import org.apache.pekko.stream.Materializer
import org.lfdecentralizedtrust.splice.automation.{
  PollingParallelTaskExecutionTrigger,
  TaskOutcome,
  TaskSuccess,
  TriggerContext,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight
import org.lfdecentralizedtrust.splice.store.AppStoreWithIngestion.SpliceLedgerConnectionPriority
import org.lfdecentralizedtrust.splice.store.PageLimit
import org.lfdecentralizedtrust.splice.util.AssignedContract
import MergeDuplicateFeaturedAppRightsTrigger.*

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

class MergeDuplicateFeaturedAppRightsTrigger(
    baseContext: TriggerContext,
    mergeInterval: NonNegativeFiniteDuration,
    override protected val svTaskContext: SvTaskBasedTrigger.Context,
)(implicit
    ec: ExecutionContext,
    tracer: Tracer,
    mat: Materializer,
) extends PollingParallelTaskExecutionTrigger[Task]
    with SvTaskBasedTrigger[Task] {

  override protected lazy val context: TriggerContext =
    baseContext.copy(config = baseContext.config.copy(pollingInterval = mergeInterval))

  private val store = svTaskContext.dsoStore

  override protected def retrieveTasks()(implicit tc: TraceContext): Future[Seq[Task]] =
    for {
      mergeSupport <- svTaskContext.packageVersionSupport.supportsFeaturedAppUnderlock(
        Seq(store.key.svParty, store.key.dsoParty),
        context.clock.now.minus(context.config.clockSkewAutomationDelay.asJava),
      )
      providers <-
        if (mergeSupport.supported)
          store.listFeaturedAppRightProvidersWithDuplicatesSample(
            PageLimit.tryCreate(context.config.parallelism)
          )
        else Future.successful(Seq.empty)
      tasks <- MonadUtil.sequentialTraverse(providers)(provider =>
        store
          .listFeaturedAppRightsForProvider(provider, maxRightsPerMerge)
          .map(Task(provider, _))
      )
    } yield tasks.filter(_.rights.sizeIs > 1)

  override protected def completeTaskAsDsoDelegate(task: Task, controller: String)(implicit
      tc: TraceContext
  ): Future[TaskOutcome] = {
    logger.warn(s"Provider ${task.provider} has ${task.rights.size} FeaturedAppRight contracts.")
    for {
      dsoRules <- store.getDsoRules()
      cmd = dsoRules.exercise(
        _.exerciseDsoRules_MergeFeaturedAppRights(
          controller,
          task.rights.map(_.contractId).asJava,
        )
      )
      _ <- svTaskContext
        .connection(SpliceLedgerConnectionPriority.Low)
        .submit(Seq(store.key.svParty), Seq(store.key.dsoParty), cmd)
        .noDedup
        .withSynchronizerId(dsoRules.domain)
        .yieldUnit()
    } yield TaskSuccess(s"Merged ${task.rights.size} featured app rights of ${task.provider}")
  }

  override protected def isStaleTask(task: Task)(implicit tc: TraceContext): Future[Boolean] =
    store.multiDomainAcsStore.containsArchived(task.rights.map(_.contractId))
}

object MergeDuplicateFeaturedAppRightsTrigger {
  private val maxRightsPerMerge = PageLimit.tryCreate(100)

  final case class Task(
      provider: PartyId,
      rights: Seq[AssignedContract[FeaturedAppRight.ContractId, FeaturedAppRight]],
  ) extends PrettyPrinting {
    override def pretty: Pretty[this.type] =
      prettyOfClass(param("provider", _.provider), param("rights", _.rights))
  }
}
