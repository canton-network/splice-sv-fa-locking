// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.sv.automation.delegatebased

import com.digitalasset.canton.tracing.TraceContext
import io.opentelemetry.api.trace.Tracer
import org.apache.pekko.stream.Materializer
import org.lfdecentralizedtrust.splice.automation.{
  PollingParallelTaskExecutionTrigger,
  TaskOutcome,
  TaskSuccess,
  TriggerContext,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.FeaturedAppRight
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.RevokeFeaturedAppRightInstruction
import org.lfdecentralizedtrust.splice.store.AppStoreWithIngestion.SpliceLedgerConnectionPriority
import org.lfdecentralizedtrust.splice.store.PageLimit
import org.lfdecentralizedtrust.splice.sv.store.SvDsoStore

import scala.concurrent.{ExecutionContext, Future}

class ExecuteRevokeFeaturedAppRightInstructionTrigger(
    override protected val context: TriggerContext,
    override protected val svTaskContext: SvTaskBasedTrigger.Context,
)(implicit
    ec: ExecutionContext,
    tracer: Tracer,
    mat: Materializer,
) extends PollingParallelTaskExecutionTrigger[
      SvDsoStore.RevokeFeaturedAppRightInstructionWithRight
    ]
    with SvTaskBasedTrigger[SvDsoStore.RevokeFeaturedAppRightInstructionWithRight] {
  private val store = svTaskContext.dsoStore

  override protected def retrieveTasks()(implicit
      tc: TraceContext
  ): Future[Seq[SvDsoStore.RevokeFeaturedAppRightInstructionWithRight]] =
    store.listRevokeFeaturedAppRightInstructionSample(
      PageLimit.tryCreate(context.config.parallelism)
    )

  override protected def completeTaskAsDsoDelegate(
      task: SvDsoStore.RevokeFeaturedAppRightInstructionWithRight,
      controller: String,
  )(implicit
      tc: TraceContext
  ): Future[TaskOutcome] =
    for {
      dsoRules <- store.getDsoRules()
      cmd = dsoRules.exercise(
        _.exerciseDsoRules_ExecuteRevokeFeaturedAppRightInstruction(
          task.instruction.contractId,
          task.featuredAppRightCid,
          controller,
        )
      )
      _ <- svTaskContext
        .connection(SpliceLedgerConnectionPriority.Low)
        .submit(Seq(store.key.svParty), Seq(store.key.dsoParty), cmd)
        .noDedup
        .withSynchronizerId(dsoRules.domain)
        .yieldUnit()
    } yield TaskSuccess(
      s"Revoked the featured app right of provider ${task.instruction.payload.provider}"
    )

  override protected def isStaleTask(
      task: SvDsoStore.RevokeFeaturedAppRightInstructionWithRight
  )(implicit tc: TraceContext): Future[Boolean] =
    for {
      instruction <- store.multiDomainAcsStore.lookupContractById(
        RevokeFeaturedAppRightInstruction.COMPANION
      )(task.instruction.contractId)
      featuredAppRight <- store.multiDomainAcsStore.lookupContractById(FeaturedAppRight.COMPANION)(
        task.featuredAppRightCid
      )
    } yield instruction.isEmpty || featuredAppRight.isEmpty
}
