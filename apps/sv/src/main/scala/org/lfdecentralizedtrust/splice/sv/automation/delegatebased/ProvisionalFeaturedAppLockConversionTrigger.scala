// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.sv.automation.delegatebased

import com.digitalasset.canton.logging.pretty.{Pretty, PrettyPrinting}
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
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock.GovernanceLock
import org.lfdecentralizedtrust.splice.store.AppStoreWithIngestion.SpliceLedgerConnectionPriority
import org.lfdecentralizedtrust.splice.store.PageLimit
import org.lfdecentralizedtrust.splice.sv.config.SvAppBackendConfig

import scala.concurrent.{ExecutionContextExecutor, Future}

import ProvisionalFeaturedAppLockConversionTrigger.*

/** Converts provisional featured app `GovernanceLock`s into real ones once their provider holds a
  * `FeaturedAppRight`.
  */
class ProvisionalFeaturedAppLockConversionTrigger(
    svConfig: SvAppBackendConfig,
    override protected val context: TriggerContext,
    svTaskContext: SvTaskBasedTrigger.Context,
)(implicit
    ec: ExecutionContextExecutor,
    mat: Materializer,
    tracer: Tracer,
) extends PollingParallelTaskExecutionTrigger[Task] {

  private val store = svTaskContext.dsoStore

  override protected def retrieveTasks()(implicit tc: TraceContext): Future[Seq[Task]] =
    store
      .listProvisionalGovernanceLocksWithFeaturedAppRightSample(
        PageLimit.tryCreate(
          svConfig.delegatelessAutomationProvisionalFeaturedAppLockConversionSampleSize
        )
      )
      .map(_.map { case (governanceLock, featuredAppRightCid) =>
        Task(governanceLock.contractId, featuredAppRightCid)
      })

  override protected def completeTask(task: Task)(implicit
      tc: TraceContext
  ): Future[TaskOutcome] = {
    val svParty = store.key.svParty
    for {
      dsoRules <- store.getDsoRules()
      cmd = dsoRules.exercise(
        _.exerciseDsoRules_GovernanceLock_ConvertProvisionalFeaturedAppLock(
          task.governanceLockCid,
          task.featuredAppRightCid,
          svParty.toProtoPrimitive,
        )
      )
      _ <- svTaskContext
        .connection(SpliceLedgerConnectionPriority.Low)
        .submit(
          Seq(svParty),
          Seq(store.key.dsoParty),
          cmd,
        )
        .noDedup
        .yieldUnit()
    } yield TaskSuccess(
      s"Converted provisional featured app lock ${task.governanceLockCid.contractId} " +
        s"using FeaturedAppRight ${task.featuredAppRightCid.contractId}"
    )
  }

  override protected def isStaleTask(task: Task)(implicit
      tc: TraceContext
  ): Future[Boolean] =
    for {
      governanceLock <- store.multiDomainAcsStore
        .lookupContractById(GovernanceLock.COMPANION)(task.governanceLockCid)
      featuredAppRight <- store.multiDomainAcsStore
        .lookupContractById(FeaturedAppRight.COMPANION)(task.featuredAppRightCid)
    } yield governanceLock.isEmpty || featuredAppRight.isEmpty
}

object ProvisionalFeaturedAppLockConversionTrigger {
  final case class Task(
      governanceLockCid: GovernanceLock.ContractId,
      featuredAppRightCid: FeaturedAppRight.ContractId,
  ) extends PrettyPrinting {

    import com.digitalasset.canton.participant.pretty.Implicits.prettyContractId

    override def pretty: Pretty[this.type] =
      prettyOfClass(
        param("governanceLockCid", _.governanceLockCid),
        param("featuredAppRightCid", _.featuredAppRightCid),
      )
  }
}
