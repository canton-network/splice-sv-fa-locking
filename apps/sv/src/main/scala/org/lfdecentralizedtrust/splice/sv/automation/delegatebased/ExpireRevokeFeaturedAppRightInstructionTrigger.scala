// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.sv.automation.delegatebased

import com.digitalasset.canton.tracing.TraceContext
import io.opentelemetry.api.trace.Tracer
import org.apache.pekko.stream.Materializer
import org.lfdecentralizedtrust.splice.automation.{
  MultiDomainExpiredContractTrigger,
  ScheduledTaskTrigger,
  TaskOutcome,
  TaskSuccess,
  TriggerContext,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.dso.instruction.RevokeFeaturedAppRightInstruction
import org.lfdecentralizedtrust.splice.store.AppStoreWithIngestion.SpliceLedgerConnectionPriority
import org.lfdecentralizedtrust.splice.util.AssignedContract
import ExpireRevokeFeaturedAppRightInstructionTrigger.*

import scala.concurrent.{ExecutionContext, Future}

class ExpireRevokeFeaturedAppRightInstructionTrigger(
    override protected val context: TriggerContext,
    override protected val svTaskContext: SvTaskBasedTrigger.Context,
)(implicit override val ec: ExecutionContext, mat: Materializer, tracer: Tracer)
    extends MultiDomainExpiredContractTrigger.Template[
      RevokeFeaturedAppRightInstruction.ContractId,
      RevokeFeaturedAppRightInstruction,
    ](
      svTaskContext.dsoStore.multiDomainAcsStore,
      svTaskContext.dsoStore.listExpiredRevokeFeaturedAppRightInstruction,
      RevokeFeaturedAppRightInstruction.COMPANION,
    )
    with SvTaskBasedTrigger[Task] {

  private val store = svTaskContext.dsoStore

  override protected def completeTaskAsDsoDelegate(task: Task, controller: String)(implicit
      tc: TraceContext
  ): Future[TaskOutcome] =
    for {
      dsoRules <- store.getDsoRules()
      cmd = dsoRules.exercise(
        _.exerciseDsoRules_ExpireRevokeFeaturedAppRightInstruction(
          task.work.contractId,
          controller,
        )
      )
      _ <- svTaskContext
        .connection(SpliceLedgerConnectionPriority.Low)
        .submit(Seq(store.key.svParty), Seq(store.key.dsoParty), cmd)
        .noDedup
        .yieldUnit()
    } yield TaskSuccess("expired revoke featured app right instruction")
}

object ExpireRevokeFeaturedAppRightInstructionTrigger {
  type Task = ScheduledTaskTrigger.ReadyTask[
    AssignedContract[
      RevokeFeaturedAppRightInstruction.ContractId,
      RevokeFeaturedAppRightInstruction,
    ]
  ]
}
