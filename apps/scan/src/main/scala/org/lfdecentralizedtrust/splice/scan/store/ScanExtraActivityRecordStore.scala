// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package org.lfdecentralizedtrust.splice.scan.store

import com.digitalasset.canton.lifecycle.CloseContext
import com.digitalasset.canton.logging.NamedLoggerFactory
import com.digitalasset.canton.resource.DbStorage
import com.digitalasset.canton.topology.{ParticipantId, PartyId}
import com.digitalasset.canton.tracing.TraceContext
import org.lfdecentralizedtrust.splice.scan.store.db.DbExtraActivityRecordStore
import org.lfdecentralizedtrust.splice.store.db.StoreDescriptor

import scala.concurrent.{ExecutionContext, Future}

object ScanExtraActivityRecordStore {
  def apply(
      dsoParty: PartyId,
      participantId: ParticipantId,
      storage: DbStorage,
      loggerFactory: NamedLoggerFactory,
  )(implicit
      ec: ExecutionContext,
      cc: CloseContext,
      tc: TraceContext,
  ): Future[DbExtraActivityRecordStore] =
    DbExtraActivityRecordStore(
      StoreDescriptor(
        version = 1,
        name = "ScanExtraActivityRecordStore",
        party = dsoParty,
        participant = participantId,
        key = Map(
          "dsoParty" -> dsoParty.toProtoPrimitive
        ),
      ),
      storage,
      loggerFactory,
    )
}
