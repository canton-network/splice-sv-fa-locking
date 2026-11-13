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
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.FeaturedAppUnderlockInstruction
import org.lfdecentralizedtrust.splice.store.AppStoreWithIngestion.SpliceLedgerConnectionPriority
import org.lfdecentralizedtrust.splice.store.PageLimit
import org.lfdecentralizedtrust.splice.sv.store.SvDsoStore

import scala.concurrent.{ExecutionContext, Future}

/** Applies each `FeaturedAppUnderlockInstruction` to a `FeaturedAppRight` of
  * its provider.
  *
  * Instructions whose provider has no right are not sampled: they cannot be
  * executed, and are left for the expiry automation.
  */
class ExecuteFeaturedAppUnderlockInstructionTrigger(
    override protected val context: TriggerContext,
    override protected val svTaskContext: SvTaskBasedTrigger.Context,
)(implicit
    ec: ExecutionContext,
    tracer: Tracer,
    mat: Materializer,
) extends PollingParallelTaskExecutionTrigger[
      SvDsoStore.FeaturedAppUnderlockInstructionWithRight
    ]
    with SvTaskBasedTrigger[
      SvDsoStore.FeaturedAppUnderlockInstructionWithRight
    ] {
  private val store = svTaskContext.dsoStore

  override protected def retrieveTasks()(implicit
      tc: TraceContext
  ): Future[Seq[SvDsoStore.FeaturedAppUnderlockInstructionWithRight]] =
    store.listFeaturedAppUnderlockInstructionSample(
      PageLimit.tryCreate(context.config.parallelism)
    )

  override protected def completeTaskAsDsoDelegate(
      task: SvDsoStore.FeaturedAppUnderlockInstructionWithRight,
      controller: String,
  )(implicit
      tc: TraceContext
  ): Future[TaskOutcome] = {
    val instruction = task.instruction.payload
    for {
      dsoRules <- store.getDsoRules()
      cmd = dsoRules.exercise(
        _.exerciseDsoRules_ExecuteFeaturedAppUnderlockInstruction(
          task.instruction.contractId,
          task.featuredAppRightCid,
          controller,
        )
      )
      _ <- svTaskContext
        .connection(SpliceLedgerConnectionPriority.Low)
        .submit(
          Seq(store.key.svParty),
          Seq(store.key.dsoParty),
          cmd,
        )
        .noDedup
        .withSynchronizerId(dsoRules.domain)
        .yieldUnit()
    } yield TaskSuccess(
      s"Applied underlock instruction " +
        s"(underlocked = ${instruction.underlocked}) " +
        s"to the FeaturedAppRight of ${instruction.provider}"
    )
  }

  override protected def isStaleTask(
      task: SvDsoStore.FeaturedAppUnderlockInstructionWithRight
  )(implicit tc: TraceContext): Future[Boolean] =
    for {
      instruction <- store.multiDomainAcsStore.lookupContractById(
        FeaturedAppUnderlockInstruction.COMPANION
      )(task.instruction.contractId)
      featuredAppRight <- store.multiDomainAcsStore.lookupContractById(
        FeaturedAppRight.COMPANION
      )(task.featuredAppRightCid)
    } yield instruction.isEmpty || featuredAppRight.isEmpty
}
