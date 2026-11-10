// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.sv.automation.confirmation

import com.digitalasset.canton.config.NonNegativeFiniteDuration
import com.digitalasset.canton.topology.PartyId
import com.digitalasset.canton.tracing.TraceContext
import com.digitalasset.canton.util.ShowUtil.*
import io.opentelemetry.api.trace.Tracer
import org.apache.pekko.stream.Materializer
import org.lfdecentralizedtrust.splice.automation.{
  PollingParallelTaskExecutionTrigger,
  TaskNoop,
  TaskOutcome,
  TaskStale,
  TaskSuccess,
  TriggerContext,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.{
  ActionRequiringConfirmation,
  DsoRules_CreateFeaturedAppUnderlockInstruction,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.actionrequiringconfirmation.ARC_DsoRules
import org.lfdecentralizedtrust.splice.codegen.java.splice.dsorules.dsorules_actionrequiringconfirmation.SRARC_CreateFeaturedAppUnderlockInstruction
import org.lfdecentralizedtrust.splice.environment.{PackageVersionSupport, SpliceLedgerConnection}
import org.lfdecentralizedtrust.splice.store.MultiDomainAcsStore.QueryResult
import org.lfdecentralizedtrust.splice.sv.store.{
  FeaturedAppUnderlockConfig,
  FeaturedAppUnderlockStatus,
  SvDsoStore,
}

import scala.concurrent.{ExecutionContext, Future}

/** Confirms the creation of a `FeaturedAppUnderlockInstruction` for each featured app provider whose
  * underlock status, computed from its governance locks, differs from the one on its `FeaturedAppRight`.
  */
class FeaturedAppUnderlockStatusUpdateTrigger(
    baseContext: TriggerContext,
    updateInterval: NonNegativeFiniteDuration,
    store: SvDsoStore,
    connection: SpliceLedgerConnection,
    packageVersionSupport: PackageVersionSupport,
)(implicit
    ec: ExecutionContext,
    mat: Materializer,
    tracer: Tracer,
) extends PollingParallelTaskExecutionTrigger[FeaturedAppUnderlockStatus] {

  override protected lazy val context: TriggerContext =
    baseContext.copy(config = baseContext.config.copy(pollingInterval = updateInterval))

  private val svParty = store.key.svParty
  private val dsoParty = store.key.dsoParty

  override def retrieveTasks()(implicit
      tc: TraceContext
  ): Future[Seq[FeaturedAppUnderlockStatus]] =
    for {
      featureSupport <- packageVersionSupport.supportsFeaturedAppUnderlock(
        Seq(svParty, dsoParty),
        context.clock.now.minus(context.config.clockSkewAutomationDelay.asJava),
      )
      statusChanges <-
        if (featureSupport.supported)
          FeaturedAppUnderlockStatusUpdateTrigger.listStatusChanges(store, provider = None)
        else Future.successful(Seq.empty)
    } yield statusChanges

  override def completeTask(
      task: FeaturedAppUnderlockStatus
  )(implicit tc: TraceContext): Future[TaskOutcome] = {
    val action = FeaturedAppUnderlockStatusUpdateTrigger.createInstructionAction(task)
    for {
      isStale <- isStaleTask(task)
      outcome <-
        if (isStale) Future.successful(TaskStale)
        else
          store.lookupConfirmationByActionWithOffset(svParty, action).flatMap {
            case QueryResult(_, Some(_)) => Future.successful(TaskNoop)
            case QueryResult(offset, None) =>
              for {
                dsoRules <- store.getDsoRules()
                _ <- connection
                  .submit(
                    actAs = Seq(svParty),
                    readAs = Seq(dsoParty),
                    update = dsoRules.exercise(
                      _.exerciseDsoRules_ConfirmAction(svParty.toProtoPrimitive, action)
                    ),
                  )
                  .withDedup(
                    commandId = SpliceLedgerConnection.CommandId(
                      "org.lfdecentralizedtrust.splice.sv.createFeaturedAppUnderlockInstructionConfirmation",
                      Seq(svParty, dsoParty, task.provider),
                      Seq(task.underlocked.toString),
                    ),
                    deduplicationOffset = offset,
                  )
                  .yieldUnit()
              } yield TaskSuccess(show"created confirmation for $task")
          }
    } yield outcome
  }

  override def isStaleTask(task: FeaturedAppUnderlockStatus)(implicit
      tc: TraceContext
  ): Future[Boolean] =
    FeaturedAppUnderlockStatusUpdateTrigger.isCurrent(store, task).map(!_)
}

object FeaturedAppUnderlockStatusUpdateTrigger {

  /** List the status changes of all featured app providers, or only of `provider`, under the
    * current `DsoRules` config.
    */
  def listStatusChanges(store: SvDsoStore, provider: Option[PartyId])(implicit
      ec: ExecutionContext,
      tc: TraceContext,
  ): Future[Seq[FeaturedAppUnderlockStatus]] =
    for {
      dsoRules <- store.getDsoRules()
      statusChanges <- store.listFeaturedAppUnderlockStatusChanges(
        FeaturedAppUnderlockConfig.fromDsoRulesConfig(dsoRules.payload.config),
        provider,
      )
    } yield statusChanges

  /** Whether `status` is still a change to make: the provider's computed status still differs from
    * the one on its right, and no instruction awaits execution.
    */
  def isCurrent(store: SvDsoStore, status: FeaturedAppUnderlockStatus)(implicit
      ec: ExecutionContext,
      tc: TraceContext,
  ): Future[Boolean] =
    listStatusChanges(store, Some(status.provider)).map(_.contains(status))

  def createInstructionAction(status: FeaturedAppUnderlockStatus): ActionRequiringConfirmation =
    new ARC_DsoRules(
      new SRARC_CreateFeaturedAppUnderlockInstruction(
        new DsoRules_CreateFeaturedAppUnderlockInstruction(
          status.provider.toProtoPrimitive,
          status.underlocked,
        )
      )
    )
}
