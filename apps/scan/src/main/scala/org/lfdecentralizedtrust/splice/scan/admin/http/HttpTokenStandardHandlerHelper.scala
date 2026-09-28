// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.scan.admin.http

import com.daml.ledger.javaapi.data.Identifier
import com.daml.ledger.javaapi.data.codegen.{ContractId, DamlRecord}
import com.digitalasset.canton.time.Clock
import com.digitalasset.canton.tracing.TraceContext
import org.lfdecentralizedtrust.splice.codegen.java.splice.amulet.LockedAmulet
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock
import org.lfdecentralizedtrust.splice.scan.store.ScanStore
import org.lfdecentralizedtrust.splice.scan.util
import org.lfdecentralizedtrust.splice.store.ChoiceContextContractFetcher

import scala.concurrent.{ExecutionContext, Future}

trait HttpTokenStandardHandlerHelper {
  protected val store: ScanStore
  protected val contractFetcher: ChoiceContextContractFetcher
  protected val clock: Clock

  protected def tokenStandardNotFound[A](
      interfaceType: String,
      cid: String,
      foundContractTemplateId: Option[Identifier],
  ): A =
    throw io.grpc.Status.NOT_FOUND
      .withDescription(
        s"$interfaceType '$cid' not found." +
          foundContractTemplateId.fold("")(templateId =>
            s" Found contract of type $templateId but it did not match a known TransferInstruction type."
          )
      )
      .asRuntimeException()

  protected def getTokenStandardWithGovernanceLockChoiceContext[
      DisclosedContract,
      ChoiceContext,
      Builder <: util.ChoiceContextBuilder[
        DisclosedContract,
        ChoiceContext,
        Builder,
      ],
  ](
      interfaceType: String,
      cid: String,
      newBuilder: String => Builder,
  )(
      matchContract: PartialFunction[DamlRecord[?], Future[ChoiceContext]]
  )(implicit ec: ExecutionContext, tc: TraceContext): Future[ChoiceContext] = {
    def getGovernanceLockContext(
        type_ : String,
        lockedAmuletId: LockedAmulet.ContractId,
        requireLockedAmulet: Boolean,
    ) =
      util.ChoiceContextBuilder.getGovernanceLockContext[
        DisclosedContract,
        ChoiceContext,
        Builder,
      ](
        s"$type_ '$cid'",
        lockedAmuletId,
        requireLockedAmulet,
        store,
        contractFetcher,
        clock,
        newBuilder,
      )

    contractFetcher
      .lookupGenericContractById(new ContractId(cid))
      .flatMap {
        case Some(contract) =>
          matchContract.applyOrElse[DamlRecord[?], Future[ChoiceContext]](
            contract.payload,
            _ =>
              contract.payload match {
                case governanceLock: governancelock.GovernanceLock =>
                  getGovernanceLockContext("GovernanceLock", governanceLock.lockedAmulet, true)
                case vestingLock: governancelock.VestingLock =>
                  // A partial withdrawal (< endTime) unlocks the LockedAmulet, so it is required.
                  // A full withdrawal (>= endTime) returns the holding directly, so doesn't need it.
                  val requireLockedAmulet = clock.now.toInstant.isBefore(vestingLock.endTime)
                  getGovernanceLockContext(
                    "VestingLock",
                    vestingLock.lockedAmulet,
                    requireLockedAmulet,
                  )
                case _ => tokenStandardNotFound(interfaceType, cid, Some(contract.identifier))
              },
          )
        case None => tokenStandardNotFound(interfaceType, cid, None)
      }
  }
}
