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
import org.lfdecentralizedtrust.splice.store.{PageLimit, UnavailablePartiesStore}
import org.lfdecentralizedtrust.splice.sv.config.SvAppBackendConfig
import org.lfdecentralizedtrust.splice.sv.util.ContractStakeholders
import org.lfdecentralizedtrust.splice.util.Contract

import scala.concurrent.{ExecutionContext, Future}

import ProvisionalFeaturedAppLockConversionTrigger.{Task, getStakeholders}

/** Converts provisional featured app `GovernanceLock`s into real ones once their provider holds a
  * `FeaturedAppRight`.
  */
class ProvisionalFeaturedAppLockConversionTrigger(
    override protected val svConfig: SvAppBackendConfig,
    override protected val context: TriggerContext,
    override protected val svTaskContext: SvTaskBasedTrigger.Context,
    override protected val unavailablePartiesStore: UnavailablePartiesStore,
)(implicit
    override val ec: ExecutionContext,
    mat: Materializer,
    tracer: Tracer,
) extends PollingParallelTaskExecutionTrigger[Task]
    with SvTaskBasedTrigger[Task]
    with UnavailablePartiesGuard {

  private val store = svTaskContext.dsoStore

  override protected def retrieveTasks()(implicit tc: TraceContext): Future[Seq[Task]] =
    store
      .listProvisionalGovernanceLocksWithFeaturedAppRightSample(
        PageLimit.tryCreate(
          svConfig.delegatelessAutomationProvisionalFeaturedAppLockConversionSampleSize
        ),
        Some(unavailablePartiesStore),
      )
      .map(_.map { case (governanceLock, featuredAppRightCid) =>
        Task(governanceLock, featuredAppRightCid)
      })

  override def completeTaskAsDsoDelegate(task: Task, controller: String)(implicit
      tc: TraceContext
  ): Future[TaskOutcome] =
    completeWithVettedAmuletVersion(
      getStakeholders(task.governanceLock.payload).toSet,
      Seq(task.governanceLock.contractId.contractId),
    )(convertLock(task, controller))

  private def convertLock(task: Task, controller: String)(implicit
      tc: TraceContext
  ): Future[TaskOutcome] =
    for {
      dsoRules <- store.getDsoRules()
      cmd = dsoRules.exercise(
        _.exerciseDsoRules_GovernanceLock_ConvertProvisionalFeaturedAppLock(
          task.governanceLock.contractId,
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
        .yieldUnit()
    } yield TaskSuccess(
      s"Converted provisional featured app lock ${task.governanceLock.contractId.contractId} " +
        s"using FeaturedAppRight ${task.featuredAppRightCid.contractId}"
    )

  override protected def isStaleTask(task: Task)(implicit
      tc: TraceContext
  ): Future[Boolean] =
    for {
      governanceLock <- store.multiDomainAcsStore
        .lookupContractById(GovernanceLock.COMPANION)(task.governanceLock.contractId)
      featuredAppRight <- store.multiDomainAcsStore
        .lookupContractById(FeaturedAppRight.COMPANION)(task.featuredAppRightCid)
    } yield governanceLock.isEmpty || featuredAppRight.isEmpty
}

object ProvisionalFeaturedAppLockConversionTrigger extends ContractStakeholders[GovernanceLock] {

  override def informees(payload: GovernanceLock): Seq[String] = Seq(payload.owner)

  override def dso(payload: GovernanceLock): String = payload.dso

  final case class Task(
      governanceLock: Contract[GovernanceLock.ContractId, GovernanceLock],
      featuredAppRightCid: FeaturedAppRight.ContractId,
  ) extends PrettyPrinting {

    import com.digitalasset.canton.participant.pretty.Implicits.prettyContractId

    override def pretty: Pretty[this.type] =
      prettyOfClass(
        param("governanceLockCid", _.governanceLock.contractId),
        param("featuredAppRightCid", _.featuredAppRightCid),
      )
  }
}
