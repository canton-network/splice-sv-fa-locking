package org.lfdecentralizedtrust.splice.integration.tests

import cats.data.NonEmptyList
import cats.syntax.foldable.*
import com.daml.ledger.javaapi.data.{Template, Unit as DamlUnit}
import com.daml.ledger.javaapi.data.codegen.{Contract, ContractId, ContractCompanion}
import com.digitalasset.canton.topology.PartyId
import io.circe.syntax.*
import org.lfdecentralizedtrust.splice.codegen.java.da.time.types.RelTime
import org.lfdecentralizedtrust.splice.codegen.java.splice.api.token.{
  metadatav1,
  transferinstructionv1 as tiv1,
  transferinstructionv2 as tiv2,
}
import org.lfdecentralizedtrust.splice.codegen.java.splice.governancelock as gl
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.console.{
  ParticipantClientReference,
  ValidatorAppBackendReference,
  WalletAppClientReference,
}
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.SpliceTestConsoleEnvironment
import org.lfdecentralizedtrust.splice.util.{
  ChoiceContextWithDisclosures,
  GovernanceLockUtil,
  WalletTestUtil,
}
import org.lfdecentralizedtrust.tokenstandard.transferinstruction

import java.time.{Duration, Instant}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait GovernanceLockTest extends TokenStandardTest with WalletTestUtil {
  private val lockCipPrefix = "cip-127"
  private val lockMemoPrefix = s"$lockCipPrefix/memo:"

  // Same as `defaultGovernanceLockMinimumLockAmount` in Daml.
  protected val minimumLockAmount = BigDecimal(10000.0)

  protected val tiaAccept = new tiv2.transferinstructionaction.TIA_Accept(DamlUnit.getInstance)
  protected val tiaReject = new tiv2.transferinstructionaction.TIA_Reject(DamlUnit.getInstance)
  protected val tiaWithdraw = new tiv2.transferinstructionaction.TIA_Withdraw(DamlUnit.getInstance)

  private def durationToRelTime(d: Duration): RelTime =
    new RelTime(TimeUnit.NANOSECONDS.toMicros(d.toNanos))

  private def instantToMicros(i: Instant): Long = ChronoUnit.MICROS.between(Instant.EPOCH, i)

  private def formatInstant(i: Instant): String =
    DateTimeFormatter.ISO_INSTANT.format(i.truncatedTo(ChronoUnit.MICROS))

  case class LockControllers(
      unlockControllers: List[List[PartyId]] = Nil,
      substitutionControllers: List[List[PartyId]] = Nil,
      vestingControllers: List[List[PartyId]] = Nil,
  ) {
    final def memoEntriesWithPrefix(prefix: String) =
      List(
        (prefix ++ GovernanceLockUtil.Memo.unlockControllersKey) -> unlockControllers,
        (prefix ++ GovernanceLockUtil.Memo.substitutionControllersKey) -> substitutionControllers,
        (prefix ++ GovernanceLockUtil.Memo.vestingControllersKey) -> vestingControllers,
      )
        .filter { case (_, controllers) => controllers.nonEmpty }
        .map { case (key, controllers) =>
          (key, controllers.map(_.map(_.toProtoPrimitive)).asJson.noSpaces)
        }

    final lazy val memoEntries = memoEntriesWithPrefix("")
  }

  sealed abstract class LockRequest[A](
      val request: String,
      val memoEntries: A => List[(String, String)],
  )
  case object CreateLock
      extends LockRequest[LockControllers](GovernanceLockUtil.Memo.createLockRequest, _.memoEntries)
  case object UnlockAndStartVesting
      extends LockRequest[(BigDecimal, Instant, Option[PartyId], LockControllers)](
        GovernanceLockUtil.Memo.unlockRequest,
        { case (unlockAmount, vestingStartTime, targetLockOwner, controllers) =>
          List(
            "unlock-amount" -> unlockAmount.toString,
            "vesting-start-time" -> formatInstant(vestingStartTime),
          ) ++
            targetLockOwner.map(p =>
              (GovernanceLockUtil.Memo.targetLockOwnerKey, p.toProtoPrimitive)
            ) ++
            controllers.memoEntries
        },
      )
  case object WithdrawVestedFunds
      extends LockRequest[(Instant, RelTime, Instant, Option[PartyId], LockControllers)](
        GovernanceLockUtil.Memo.withdrawRequest,
        { case (vestedUntilTime, vestingPeriod, vestingEndTime, targetLockOwner, controllers) =>
          List(
            "vested-until-time" -> formatInstant(vestedUntilTime),
            GovernanceLockUtil.Memo.vestingDurationKey -> vestingPeriod.microseconds.toString,
            GovernanceLockUtil.Memo.vestingEndTimeKey -> instantToMicros(vestingEndTime).toString,
          ) ++
            targetLockOwner.map(p =>
              (GovernanceLockUtil.Memo.targetLockOwnerKey, p.toProtoPrimitive)
            ) ++
            controllers.memoEntries
        },
      )

  case object SubstituteLock
      extends LockRequest[
        (
            GovernanceLockUtil.LockStatus,
            PartyId,
            Option[RelTime],
            Option[Instant],
            LockControllers,
            LockControllers,
        )
      ](
        GovernanceLockUtil.Memo.substituteRequest,
        {
          case (
                lockStatus,
                targetLockOwner,
                vestingPeriod,
                vestingEndTime,
                targetControllers,
                newControllers,
              ) =>
            List(
              GovernanceLockUtil.Memo.lockStatusKey -> lockStatus.status,
              GovernanceLockUtil.Memo.targetLockOwnerKey -> targetLockOwner.toProtoPrimitive,
            ) ++
              vestingPeriod.map(t =>
                (GovernanceLockUtil.Memo.vestingDurationKey, t.microseconds.toString)
              ) ++
              vestingEndTime.map(t =>
                (GovernanceLockUtil.Memo.vestingEndTimeKey, instantToMicros(t).toString)
              ) ++
              targetControllers.memoEntries ++
              newControllers.memoEntriesWithPrefix("new-")
        },
      )

  case object TransferLock
      extends LockRequest[
        (
            GovernanceLockUtil.LockStatus,
            PartyId,
            BigDecimal,
            Option[RelTime],
            Option[Instant],
            Option[PartyId],
            LockControllers,
            LockControllers,
        )
      ](
        GovernanceLockUtil.Memo.transferRequest,
        {
          case (
                lockStatus,
                newLockOwner,
                transferAmount,
                vestingPeriod,
                vestingEndTime,
                targetLockOwner,
                sourceControllers,
                newControllers,
              ) =>
            List(
              GovernanceLockUtil.Memo.lockStatusKey -> lockStatus.status,
              "new-lock-owner" -> newLockOwner.toProtoPrimitive,
              "transfer-amount" -> transferAmount.toString,
            ) ++
              vestingPeriod.map(t =>
                (GovernanceLockUtil.Memo.vestingDurationKey, t.microseconds.toString)
              ) ++
              vestingEndTime.map(t =>
                (GovernanceLockUtil.Memo.vestingEndTimeKey, instantToMicros(t).toString)
              ) ++
              targetLockOwner.map(p =>
                (GovernanceLockUtil.Memo.targetLockOwnerKey, p.toProtoPrimitive)
              ) ++
              sourceControllers.memoEntries ++
              newControllers.memoEntriesWithPrefix("new-")
        },
      )

  private def makeLockMemo(entries: List[(String, String)]): String =
    lockMemoPrefix + entries.map { case (k, v) => s"$k=$v" }.mkString("&")

  private def commonLockMemoEntries(
      lockKind: GovernanceLockUtil.LockKind,
      lockSubject: String,
  ): List[(String, String)] =
    List(
      GovernanceLockUtil.Memo.lockKindKey -> lockKind.kind,
      GovernanceLockUtil.Memo.lockSubjectKey -> lockSubject,
    )

  def makeInputLockMemo[A](
      lockRequest: LockRequest[A],
      lockKind: GovernanceLockUtil.LockKind,
      lockSubject: String,
  )(data: A): String =
    makeLockMemo(
      List(GovernanceLockUtil.Memo.requestKey -> lockRequest.request) ++
        commonLockMemoEntries(lockKind, lockSubject) ++
        lockRequest.memoEntries(data)
    )

  def makeOutputLockMemo(
      lockKind: GovernanceLockUtil.LockKind,
      lockSubject: String,
      lockStatus: GovernanceLockUtil.LockStatus,
  ): String =
    makeLockMemo(
      commonLockMemoEntries(lockKind, lockSubject) ++
        Map(GovernanceLockUtil.Memo.lockStatusKey -> lockStatus.status)
    )

  /** The memo of a pending transfer of a lock that is not vesting */
  def makeAcceptTransferLockMemo(
      lockKind: GovernanceLockUtil.LockKind,
      lockSubject: String,
  ): String =
    makeLockMemo(
      List(GovernanceLockUtil.Memo.requestKey -> "accept-to-transfer-lock") ++
        commonLockMemoEntries(lockKind, lockSubject) ++
        Map(GovernanceLockUtil.Memo.lockStatusKey -> GovernanceLockUtil.LockStatus.Locked.status)
    )

  // Using a Map so order doesn't matter when comparing below
  private def extractLockMemoEntries(memo: String): Map[String, String] =
    GovernanceLockUtil.Memo.parseFields(memo).getOrElse(Map.empty)

  def isVestingLock(lock: gl.GovernanceLock): Boolean =
    GovernanceLockUtil.getVestingLockSpecification(lock).isDefined

  def vestingLockSpecification(lock: gl.GovernanceLock): gl.VestingLockSpecification =
    GovernanceLockUtil.getVestingLockSpecification(lock).value

  def compareLockMemo(actual: String, expected: String) = {
    val actualEntries = extractLockMemoEntries(actual)
    val expectedEntries = extractLockMemoEntries(expected)

    actualEntries shouldBe expectedEntries withClue s"actual: $actual, expected: $expected"
  }

  trait TransferValues[CId, View] {
    def sender(transfer: View): String
    def receiver(transfer: View): String
    def amount(transfer: View): BigDecimal
    def meta(transfer: View): metadatav1.Metadata
    def pendingActions(transfer: View): Map[tiv2.TransferInstructionAction, Set[String]]
    def originalInstructionCid(transfer: View): Option[CId]
  }

  object TransferValues {
    implicit val transferValuesV1
        : TransferValues[tiv1.TransferInstruction.ContractId, tiv1.TransferInstructionView] =
      new TransferValues[tiv1.TransferInstruction.ContractId, tiv1.TransferInstructionView] {
        def sender(transfer: tiv1.TransferInstructionView): String =
          transfer.transfer.sender
        def receiver(transfer: tiv1.TransferInstructionView): String =
          transfer.transfer.receiver
        def amount(transfer: tiv1.TransferInstructionView): BigDecimal =
          BigDecimal(transfer.transfer.amount)
        def meta(transfer: tiv1.TransferInstructionView): metadatav1.Metadata =
          transfer.transfer.meta
        def pendingActions(transfer: tiv1.TransferInstructionView) =
          transfer.status match {
            case p: tiv1.transferinstructionstatus.TransferPendingInternalWorkflow =>
              p.pendingActions.asScala.toList.foldMap { case (party, actions) =>
                actions.split(';').toList.foldMap {
                  case a if a.contains("accept") => Map(tiaAccept -> Set(party))
                  case a if a.contains("reject") => Map(tiaReject -> Set(party))
                  case a if a.contains("withdraw") => Map(tiaWithdraw -> Set(party))
                  case _ => Map.empty
                }
              }
            case _ => Map.empty
          }
        def originalInstructionCid(transfer: tiv1.TransferInstructionView) =
          transfer.originalInstructionCid.toScala
      }

    implicit val transferValuesV2
        : TransferValues[tiv2.TransferInstruction.ContractId, tiv2.TransferInstructionView] =
      new TransferValues[tiv2.TransferInstruction.ContractId, tiv2.TransferInstructionView] {
        def sender(transfer: tiv2.TransferInstructionView): String =
          transfer.transfer.sender.owner.toScala.value
        def receiver(transfer: tiv2.TransferInstructionView): String =
          transfer.transfer.receiver.owner.toScala.value
        def amount(transfer: tiv2.TransferInstructionView): BigDecimal =
          BigDecimal(transfer.transfer.amount)
        def meta(transfer: tiv2.TransferInstructionView): metadatav1.Metadata =
          transfer.transfer.meta
        def pendingActions(transfer: tiv2.TransferInstructionView) =
          transfer.availableActions.asScala.toMap.map { case (action, parties) =>
            (action, parties.asScala.toList.flatMap(_.asScala.toList).toSet)
          }
        def originalInstructionCid(transfer: tiv2.TransferInstructionView) =
          transfer.originalInstructionCid.toScala
      }
  }

  sealed abstract class TokenStandardOps[CId, View](val version: Int)(implicit
      val transferValues: TransferValues[CId, View]
  ) {
    def makeCId(cid: String): CId

    def executeTransfer(
        participant: ParticipantClientReference,
        senders: NonEmptyList[RichPartyId],
        receiver: PartyId,
        amount: BigDecimal,
        description: Option[String],
        wait: Boolean = true,
    )(implicit env: SpliceTestConsoleEnvironment): Unit

    def acceptTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: CId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit

    def rejectTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: CId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit

    def withdrawTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: CId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit

    def listTransfers(
        participant: ParticipantClientReference,
        party: PartyId,
    ): Seq[(CId, View)]

    def getAcceptContext(cid: CId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures

    def getRejectContext(cid: CId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures

    def getWithdrawContext(cid: CId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures

    private def defaultIsValid(receiver: PartyId, existingCids: Set[CId]): (CId, View) => Boolean =
      (cid, view) =>
        transferValues.receiver(view) == receiver.toProtoPrimitive && !existingCids.contains(cid)

    private def transferAndList(
        actClue: String,
        checkClue: String,
        participant: ParticipantClientReference,
        proposers: NonEmptyList[RichPartyId],
        amount: BigDecimal,
        description: String,
        wait: Boolean,
        isValid: (CId, View) => Boolean,
    )(implicit env: SpliceTestConsoleEnvironment): Seq[(CId, View)] =
      actAndCheck(
        actClue,
        executeTransfer(
          participant,
          proposers,
          GovernanceLockUtil.lockManagerParty,
          amount,
          description = Some(description),
          wait = wait,
        ),
      )(
        checkClue,
        _ => listTransfers(participant, proposers.head.partyId).filter(isValid.tupled),
      )._2

    final def createGovernanceLock(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        lockKind: GovernanceLockUtil.LockKind,
        lockSubject: String,
        amount: BigDecimal,
        controllers: LockControllers,
    )(implicit env: SpliceTestConsoleEnvironment): CId = {
      // `owner` may already have pending instructions, so only look at new ones
      val existingCids = listTransfers(participant, owner.partyId).map(_._1).toSet

      transferAndList(
        "the owner creates the GovernanceLock",
        "a GovernanceLock is now the pending TransferInstruction",
        participant,
        NonEmptyList.one(owner),
        amount,
        makeInputLockMemo(CreateLock, lockKind, lockSubject)(controllers),
        wait = true,
        defaultIsValid(GovernanceLockUtil.lockManagerParty, existingCids),
      ).loneElement._1
    }

    private def atMostOneVestingLock(locks: Seq[(CId, View)]): Option[(CId, View)] =
      locks match {
        case Seq() => None
        case Seq(cid) => Some(cid)
        case many => fail(s"Expected at most one vesting lock, got $many")
      }

    final def unlockGovernanceLock(
        participant: ParticipantClientReference,
        proposers: NonEmptyList[RichPartyId],
        lockKind: GovernanceLockUtil.LockKind,
        lockSubject: String,
        unlockAmount: BigDecimal,
        vestingStartTime: Instant,
        targetLockOwner: Option[PartyId],
        controllers: LockControllers,
        isValid: View => Boolean = _ => true,
    )(implicit env: SpliceTestConsoleEnvironment): Option[(CId, View)] = {
      // The proposer may already have pending instructions, so only look at new ones
      val existingCids = listTransfers(participant, proposers.head.partyId).map(_._1).toSet

      atMostOneVestingLock(
        transferAndList(
          "request an unlock of the GovernanceLock",
          "a new pending TransferInstruction is created",
          participant,
          proposers,
          0,
          makeInputLockMemo(UnlockAndStartVesting, lockKind, lockSubject)(
            (unlockAmount, vestingStartTime, targetLockOwner, controllers)
          ),
          wait = false,
          (cid, view) =>
            defaultIsValid(GovernanceLockUtil.lockManagerParty, existingCids)(cid, view) &&
              isValid(view),
        )
      )
    }

    final def withdrawVestingLock(
        participant: ParticipantClientReference,
        proposers: NonEmptyList[RichPartyId],
        lockKind: GovernanceLockUtil.LockKind,
        lockSubject: String,
        vestingLock: gl.GovernanceLock,
        vestedUntilTime: Instant,
        targetLockOwner: Option[PartyId],
        controllers: LockControllers,
    )(implicit env: SpliceTestConsoleEnvironment): Option[(CId, View)] = {
      // The proposer may already have pending instructions, so only look at new ones
      val existingCids = listTransfers(participant, proposers.head.partyId).map(_._1).toSet

      atMostOneVestingLock(
        transferAndList(
          "withdraw the vesting lock",
          "at most one new pending TransferInstruction is created",
          participant,
          proposers,
          0,
          makeInputLockMemo(WithdrawVestedFunds, lockKind, lockSubject)(
            (
              vestedUntilTime,
              vestingLockSpecification(vestingLock).vestingPeriod,
              vestingLockSpecification(vestingLock).endTime,
              targetLockOwner,
              controllers,
            )
          ),
          wait = false,
          defaultIsValid(GovernanceLockUtil.lockManagerParty, existingCids),
        )
      )
    }

    final class SubstituteLock(
        lockStatus: GovernanceLockUtil.LockStatus,
        targetLockOwner: PartyId,
        vestingPeriod: Option[RelTime],
        vestingEndTime: Option[Instant],
    ) {
      final def apply(
          participant: ParticipantClientReference,
          proposer: RichPartyId,
          lockKind: GovernanceLockUtil.LockKind,
          lockSubject: String,
          substitutionAmount: BigDecimal,
          targetControllers: LockControllers,
          newControllers: LockControllers,
      )(implicit env: SpliceTestConsoleEnvironment): (CId, View) = {
        // The proposer may already have pending instructions, so only look at new ones
        val existingCids = listTransfers(participant, proposer.partyId).map(_._1).toSet

        transferAndList(
          "propose a substitution",
          "the substitution is now a pending TransferInstruction to the owner of the target lock",
          participant,
          NonEmptyList.one(proposer),
          substitutionAmount,
          makeInputLockMemo(SubstituteLock, lockKind, lockSubject)(
            (
              lockStatus,
              targetLockOwner,
              vestingPeriod,
              vestingEndTime,
              targetControllers,
              newControllers,
            )
          ),
          // A substitution proposed by the owner of the target lock locks no funds,
          // so no input holding is archived
          wait = proposer.partyId != targetLockOwner,
          defaultIsValid(targetLockOwner, existingCids),
        ).loneElement
      }
    }

    /** Propose a substitution of a GovernanceLock with the given owner */
    def substituteGovernanceLock(owner: PartyId): SubstituteLock =
      new SubstituteLock(GovernanceLockUtil.LockStatus.Locked, owner, None, None)

    /** Propose a substitution of a GovernanceLock */
    def substituteGovernanceLock(lock: gl.GovernanceLock): SubstituteLock =
      substituteGovernanceLock(PartyId.tryFromProtoPrimitive(lock.owner))

    /** Propose a substitution of a vesting GovernanceLock with the given attributes */
    def substituteVestingLock(owner: PartyId, vestingPeriod: Duration, vestingEndTime: Instant) =
      new SubstituteLock(
        GovernanceLockUtil.LockStatus.Vesting,
        owner,
        Some(durationToRelTime(vestingPeriod)),
        Some(vestingEndTime),
      )

    /** Propose a substitution of a vesting GovernanceLock */
    def substituteVestingLock(lock: gl.GovernanceLock) =
      new SubstituteLock(
        GovernanceLockUtil.LockStatus.Vesting,
        PartyId.tryFromProtoPrimitive(lock.owner),
        Some(vestingLockSpecification(lock).vestingPeriod),
        Some(vestingLockSpecification(lock).endTime),
      )

    final class TransferLock(
        lockStatus: GovernanceLockUtil.LockStatus,
        vestingPeriod: Option[RelTime],
        vestingEndTime: Option[Instant],
    ) {
      final def apply(
          participant: ParticipantClientReference,
          owner: RichPartyId,
          lockKind: GovernanceLockUtil.LockKind,
          lockSubject: String,
          newLockOwner: PartyId,
          targetLockOwner: Option[PartyId],
          transferAmount: BigDecimal,
          sourceControllers: LockControllers,
          newControllers: LockControllers,
      )(implicit env: SpliceTestConsoleEnvironment): (CId, View) = {
        // `owner` may already have pending instructions, so only look at new ones
        val existingCids = listTransfers(participant, owner.partyId).map(_._1).toSet

        transferAndList(
          "the owner proposes the lock transfer",
          "the lock transfer is now a pending TransferInstruction to the new owner",
          participant,
          NonEmptyList.one(owner),
          0,
          makeInputLockMemo(TransferLock, lockKind, lockSubject)(
            (
              lockStatus,
              newLockOwner,
              transferAmount,
              vestingPeriod,
              vestingEndTime,
              targetLockOwner,
              sourceControllers,
              newControllers,
            )
          ),
          // A lock transfer proposal moves no funds, so no input holding is archived
          wait = false,
          defaultIsValid(newLockOwner, existingCids),
        ).loneElement
      }
    }

    /** Propose a transfer of a GovernanceLock */
    final val transferGovernanceLock =
      new TransferLock(GovernanceLockUtil.LockStatus.Locked, None, None)

    /** Propose a transfer of a vesting GovernanceLock */
    final def transferVestingLock(vestingPeriod: Duration, vestingEndTime: Instant) =
      new TransferLock(
        GovernanceLockUtil.LockStatus.Vesting,
        Some(durationToRelTime(vestingPeriod)),
        Some(vestingEndTime),
      )

    /** Propose a transfer of part of a vesting GovernanceLock, keeping its
      * vesting schedule
      */
    final def transferVestingLock(lock: gl.GovernanceLock) =
      new TransferLock(
        GovernanceLockUtil.LockStatus.Vesting,
        Some(vestingLockSpecification(lock).vestingPeriod),
        Some(vestingLockSpecification(lock).endTime),
      )
  }

  object tsv1Operations
      extends TokenStandardOps[
        tiv1.TransferInstruction.ContractId,
        tiv1.TransferInstructionView,
      ](1) {
    def makeCId(cid: String) = new tiv1.TransferInstruction.ContractId(cid)

    def executeTransfer(
        participant: ParticipantClientReference,
        senders: NonEmptyList[RichPartyId],
        receiver: PartyId,
        amount: BigDecimal,
        description: Option[String],
        wait: Boolean = true,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      executeTransferViaTokenStandardV1(
        participant,
        senders.head,
        receiver,
        amount,
        transferinstruction.v1.definitions.TransferFactoryWithChoiceContext.TransferKind.Offer,
        description = description,
        wait = wait,
      )

    def acceptTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: tiv1.TransferInstruction.ContractId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      acceptTransferInstructionV1(participant, receiver, cid)

    def rejectTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: tiv1.TransferInstruction.ContractId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      rejectTransferInstructionV1(participant, receiver, cid)

    def withdrawTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: tiv1.TransferInstruction.ContractId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      withdrawTransferInstructionV1(participant, receiver, cid)

    def listTransfers(participant: ParticipantClientReference, party: PartyId) =
      listTransferInstructions(participant, party)

    def getAcceptContext(cid: tiv1.TransferInstruction.ContractId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures =
      sv1ScanBackend.getTransferInstructionAcceptContext(cid)

    def getRejectContext(cid: tiv1.TransferInstruction.ContractId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures =
      sv1ScanBackend.getTransferInstructionRejectContext(cid)

    def getWithdrawContext(cid: tiv1.TransferInstruction.ContractId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures =
      sv1ScanBackend.getTransferInstructionWithdrawContext(cid)
  }

  object tsv2Operations
      extends TokenStandardOps[
        tiv2.TransferInstruction.ContractId,
        tiv2.TransferInstructionView,
      ](2) {
    def makeCId(cid: String) = new tiv2.TransferInstruction.ContractId(cid)

    def executeTransfer(
        participant: ParticipantClientReference,
        senders: NonEmptyList[RichPartyId],
        receiver: PartyId,
        amount: BigDecimal,
        description: Option[String],
        wait: Boolean = true,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      executeTransferViaTokenStandardV2(
        participant,
        senders.head,
        receiver,
        amount,
        transferinstruction.v2.definitions.TransferFactoryWithChoiceContext.TransferKind.Offer,
        actors = Some(senders.map(_.partyId).toList),
        description = description,
        wait = wait,
      )

    def acceptTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: tiv2.TransferInstruction.ContractId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      acceptTransferInstructionV2(participant, receiver, cid)

    def rejectTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: tiv2.TransferInstruction.ContractId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      rejectTransferInstructionV2(participant, receiver, cid)

    def withdrawTransfer(
        participant: ParticipantClientReference,
        receiver: RichPartyId,
        cid: tiv2.TransferInstruction.ContractId,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      withdrawTransferInstructionV2(participant, receiver, cid)

    def listTransfers(participant: ParticipantClientReference, party: PartyId) =
      listTransferInstructionsV2(participant, party)

    def getAcceptContext(cid: tiv2.TransferInstruction.ContractId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures =
      sv1ScanBackend.getTransferInstructionAcceptContextV2(cid)

    def getRejectContext(cid: tiv2.TransferInstruction.ContractId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures =
      sv1ScanBackend.getTransferInstructionRejectContextV2(cid)

    def getWithdrawContext(cid: tiv2.TransferInstruction.ContractId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures =
      sv1ScanBackend.getTransferInstructionWithdrawContextV2(cid)
  }

  object customControllers {
    case class Party(richPartyId: RichPartyId, participant: ParticipantClientReference) {
      final val partyId: PartyId = richPartyId.partyId
    }
    case class Parties(alice: Party, bob: Party, charlie: Party, sv1: Party) {
      final val all = List(alice, bob, charlie, sv1)
    }

    private def makeParty(
        walletAppClient: WalletAppClientReference,
        validator: ValidatorAppBackendReference,
    ): Party =
      Party(
        RichPartyId.local(onboardWalletUser(walletAppClient, validator)),
        validator.participantClientWithAdminToken,
      )

    def testProposal[
        ICId <: ContractId[?],
        IView,
        TCId <: ContractId[T],
        TC <: Contract[TCId, T],
        T <: Template,
    ](
        ops: TokenStandardOps[ICId, IView],
        proposalCompanion: ContractCompanion[TC, TCId, T],
    )(
        // The custom controllers to use when testing the proposal
        makeControllers: Parties => List[List[Party]],
        // How to set the custom controllers in the `LockControllers`
        makeLockControllers: List[List[PartyId]] => LockControllers,
        // Create the lock; return its owner and its contract id
        setupLock: (Parties, LockControllers) => (Party, gl.GovernanceLock.ContractId),
        // The set of parties who propose the proposal when testing acceptance
        makeAcceptProposers: Parties => NonEmptyList[Party],
        // The set of parties who accept the proposal when testing acceptance
        makeAcceptors: Parties => NonEmptyList[Party],
        // The set of parties who propose the proposal when testing rejection
        makeRejectProposers: Parties => NonEmptyList[Party],
        // The set of parties who reject the proposal when testing rejection
        makeRejectors: Parties => NonEmptyList[Party],
        // The set of parties who propose the proposal when testing withdrawal
        makeWithdrawProposers: Parties => NonEmptyList[Party],
        // Create the proposal; return its contract id and transfer instruction view
        propose: (
            Parties,
            ParticipantClientReference,
            NonEmptyList[RichPartyId],
            LockControllers,
        ) => (ICId, IView),
        // The amount expected to be reported on the proposal's transfer instruction view
        expectedAmount: BigDecimal,
        // The receiver expected to be reported on the proposal's transfer instruction view
        expectedReceiver: Parties => PartyId,
        // The set of parties that are expected to own locks after the proposal is accepted
        makeUpdatedLockOwners: Parties => List[Party],
        // A function to check the updated set of locks after the proposal is accepted
        checkUpdatedLocks: (Parties, Seq[(ICId, IView, gl.GovernanceLock.Contract)]) => Unit,
    )(implicit env: SpliceTestConsoleEnvironment): Seq[gl.GovernanceLock.Contract] = {
      // Lock and proposal setup
      // -----------------------

      val parties = Parties(
        alice = makeParty(aliceWalletClient, aliceValidatorBackend),
        bob = makeParty(bobWalletClient, bobValidatorBackend),
        charlie = makeParty(charlieWalletClient, aliceValidatorBackend),
        sv1 = makeParty(sv1WalletClient, sv1ValidatorBackend),
      )
      val controllers = makeLockControllers(makeControllers(parties).map(_.map(_.partyId)))
      val (lockOwner, origLockCid) = setupLock(parties, controllers)

      def setupProposal(
          makeProposers: Parties => NonEmptyList[Party]
      ): (ICId, NonEmptyList[Party], List[Party]) = {
        val proposers = ops match {
          // TSv1 can only have one proposer
          case `tsv1Operations` => NonEmptyList.one(makeProposers(parties).head)
          case `tsv2Operations` => makeProposers(parties).sortBy(_.partyId.toProtoPrimitive)
        }
        val firstProposer = proposers.head

        // all proposers must be hosted on the same validator
        proposers.map(_.participant.name).toList.toSet.size shouldBe 1

        val proposersSet = proposers.toList.map(_.partyId).toSet
        val nonProposers = parties.all.filterNot(p => proposersSet.contains(p.partyId))

        // the proposers initiate the proposal
        val (proposalCid, proposalView) = propose(
          parties,
          firstProposer.participant,
          proposers.map(_.richPartyId),
          controllers,
        )
        // Prove that proposalCid is the correct proposal type
        val _ = firstProposer.participant.ledger_api_extensions.acs
          .filterJava(proposalCompanion)(
            firstProposer.partyId,
            predicate = _.id.contractId == proposalCid.contractId,
          )
          .loneElement

        ops.transferValues.sender(proposalView) shouldBe firstProposer.partyId.toProtoPrimitive
        ops.transferValues.receiver(proposalView) shouldBe
          expectedReceiver(parties).toProtoPrimitive
        ops.transferValues.amount(proposalView) shouldBe expectedAmount
        ops.transferValues.pendingActions(proposalView) shouldBe
          Map(
            tiaAccept -> nonProposers.map(_.partyId.toProtoPrimitive).toSet,
            tiaReject -> nonProposers.map(_.partyId.toProtoPrimitive).toSet,
            tiaWithdraw -> proposers.toList.map(_.partyId.toProtoPrimitive).toSet,
          )

        (proposalCid, proposers, nonProposers)
      }

      // Proposal rejection
      // ------------------

      val (proposalBeforeRejectCid, rejectProposers, rejectNonProposers) =
        setupProposal(makeRejectProposers)

      clue("Scan serves a reject choice context for the proposal") {
        ops.getRejectContext(proposalBeforeRejectCid).disclosedContracts should not be empty
      }

      val rejectors = makeRejectors(parties)
      rejectors.zipWithIndex.foldLeft((proposalBeforeRejectCid, Set.empty[PartyId])) {
        case ((accProposalCid, accRejectors), (rejector, idx)) =>
          tsv2Operations.rejectTransfer(
            rejector.participant,
            rejector.richPartyId,
            tsv2Operations.makeCId(accProposalCid.contractId),
          )

          if (idx == rejectors.length - 1) {
            // the final rejection archives the proposal and leaves the lock unchanged
            val (lockAfterRejectCid, _) =
              ops.listTransfers(lockOwner.participant, lockOwner.partyId).loneElement
            lockAfterRejectCid.contractId shouldBe origLockCid.contractId

            (lockAfterRejectCid, accRejectors)
          } else {
            // the proposal remains pending after rejection
            val updRejectors = accRejectors + rejector.partyId
            val (stillPendingProposalCid, stillPendingProposalView) = ops
              .listTransfers(rejectProposers.head.participant, rejectProposers.head.partyId)
              .filter { case (_, view) =>
                ops.transferValues.originalInstructionCid(view) == Some(proposalBeforeRejectCid)
              }
              .loneElement
            // prove that the proposal is the correct type
            val _ =
              rejectProposers.head.participant.ledger_api_extensions.acs
                .filterJava(proposalCompanion)(
                  rejectProposers.head.partyId,
                  predicate = _.id.contractId == stillPendingProposalCid.contractId,
                )
                .loneElement

            ops.transferValues.sender(stillPendingProposalView) shouldBe
              rejectProposers.head.partyId.toProtoPrimitive
            ops.transferValues.receiver(stillPendingProposalView) shouldBe
              expectedReceiver(parties).toProtoPrimitive
            ops.transferValues.amount(stillPendingProposalView) shouldBe expectedAmount
            ops.transferValues.pendingActions(stillPendingProposalView) shouldBe
              Map(
                tiaAccept -> rejectNonProposers.map(_.partyId.toProtoPrimitive).toSet,
                tiaReject -> rejectNonProposers
                  .filterNot(p => updRejectors.contains(p.partyId))
                  .map(_.partyId.toProtoPrimitive)
                  .toSet,
                tiaWithdraw -> rejectProposers.toList.map(_.partyId.toProtoPrimitive).toSet,
              )

            (stillPendingProposalCid, updRejectors)
          }
      }

      // Proposal withdrawal
      // -------------------

      val (proposalBeforeWithdrawCid, withdrawProposers, withdrawNonProposers) =
        setupProposal(makeWithdrawProposers)

      clue("Scan serves a withdraw choice context for the proposal") {
        ops.getWithdrawContext(proposalBeforeWithdrawCid).disclosedContracts should not be empty
      }

      withdrawProposers.zipWithIndex.foldLeft((proposalBeforeWithdrawCid, Set.empty[PartyId])) {
        case ((accProposalCid, accWithdrawers), (proposer, idx)) =>
          ops.withdrawTransfer(proposer.participant, proposer.richPartyId, accProposalCid)

          if (idx == withdrawProposers.length - 1) {
            // the final withdrawal archives the proposal and leaves to lock unchanged
            val (lockAfterWithdrawCid, _) =
              ops.listTransfers(lockOwner.participant, lockOwner.partyId).loneElement
            lockAfterWithdrawCid.contractId shouldBe origLockCid.contractId

            (lockAfterWithdrawCid, accWithdrawers)
          } else {
            // the proposal remains pending after withdrawal
            val updWithdrawers = accWithdrawers + proposer.partyId
            val (stillPendingProposalCid, stillPendingProposalView) = ops
              .listTransfers(proposer.participant, proposer.partyId)
              .filter { case (_, view) =>
                ops.transferValues.originalInstructionCid(view) == Some(proposalBeforeWithdrawCid)
              }
              .loneElement
            // prove that the proposal is the correct type
            val _ =
              proposer.participant.ledger_api_extensions.acs
                .filterJava(proposalCompanion)(
                  proposer.partyId,
                  predicate = _.id.contractId == stillPendingProposalCid.contractId,
                )
                .loneElement

            ops.transferValues.sender(stillPendingProposalView) shouldBe
              withdrawProposers.head.partyId.toProtoPrimitive
            ops.transferValues.receiver(stillPendingProposalView) shouldBe
              expectedReceiver(parties).toProtoPrimitive
            ops.transferValues.amount(stillPendingProposalView) shouldBe expectedAmount
            ops.transferValues.pendingActions(stillPendingProposalView) shouldBe
              Map(
                tiaAccept ->
                  (accWithdrawers + proposer.partyId ++ withdrawNonProposers.map(_.partyId))
                    .map(_.toProtoPrimitive)
                    .toSet,
                tiaReject -> withdrawNonProposers.map(_.partyId.toProtoPrimitive).toSet,
                tiaWithdraw -> withdrawProposers
                  .filterNot(p => updWithdrawers.contains(p.partyId))
                  .map(_.partyId.toProtoPrimitive)
                  .toSet,
              )

            (stillPendingProposalCid, updWithdrawers)
          }
      }

      // Proposal acceptance
      // -------------------

      val (proposalBeforeAcceptCid, acceptProposers, acceptNonProposers) =
        setupProposal(makeAcceptProposers)

      clue("Scan serves an accept choice context for the proposal") {
        ops.getAcceptContext(proposalBeforeAcceptCid).disclosedContracts should not be empty
      }

      val acceptors = makeAcceptors(parties)
      val (_, _, locks) = acceptors.zipWithIndex.foldLeft(
        (
          proposalBeforeAcceptCid,
          Set.empty[PartyId],
          Seq.empty[gl.GovernanceLock.Contract],
        )
      ) { case ((accProposalCid, accAcceptors, accLocks), (acceptor, idx)) =>
        tsv2Operations.acceptTransfer(
          acceptor.participant,
          acceptor.richPartyId,
          tsv2Operations.makeCId(accProposalCid.contractId),
        )

        if (idx == acceptors.length - 1) {
          // the final acceptance triggers execution of the proposal
          val locks = makeUpdatedLockOwners(parties)
            .flatMap { lockHolder =>
              val transferCidToView: Map[String, (ICId, IView)] =
                ops
                  .listTransfers(lockHolder.participant, lockHolder.partyId)
                  .map { case (cid, view) => (cid.contractId, (cid, view)) }
                  .toMap

              lockHolder.participant.ledger_api_extensions.acs
                .filterJava(gl.GovernanceLock.COMPANION)(
                  lockHolder.partyId,
                  predicate = l => transferCidToView.contains(l.id.contractId),
                )
                .map { lock =>
                  val (cid, view) = transferCidToView(lock.id.contractId)
                  (cid, view, lock)
                }
            }
            .distinctBy(_._1.contractId)

          checkUpdatedLocks(parties, locks)
          (accProposalCid, accAcceptors, locks.map(_._3))
        } else {
          // the proposal remains pending after acceptance
          val updAcceptors = accAcceptors + acceptor.partyId
          val (stillPendingProposalCid, stillPendingProposalView) = ops
            .listTransfers(acceptProposers.head.participant, acceptProposers.head.partyId)
            .filter { case (_, view) =>
              ops.transferValues.originalInstructionCid(view) == Some(proposalBeforeAcceptCid)
            }
            .loneElement
          // prove that the proposal is the correct type
          val _ =
            acceptProposers.head.participant.ledger_api_extensions.acs
              .filterJava(proposalCompanion)(
                acceptProposers.head.partyId,
                predicate = _.id.contractId == stillPendingProposalCid.contractId,
              )
              .loneElement

          ops.transferValues.sender(stillPendingProposalView) shouldBe
            acceptProposers.head.partyId.toProtoPrimitive
          ops.transferValues.receiver(stillPendingProposalView) shouldBe
            expectedReceiver(parties).toProtoPrimitive
          ops.transferValues.amount(stillPendingProposalView) shouldBe expectedAmount
          ops.transferValues.pendingActions(stillPendingProposalView) shouldBe
            Map(
              tiaAccept -> acceptNonProposers
                .filterNot(p => updAcceptors.contains(p.partyId))
                .map(_.partyId.toProtoPrimitive)
                .toSet,
              tiaReject -> acceptNonProposers.map(_.partyId.toProtoPrimitive).toSet,
              tiaWithdraw -> acceptProposers.toList.map(_.partyId.toProtoPrimitive).toSet,
            )

          (stillPendingProposalCid, updAcceptors, accLocks)
        }
      }

      locks
    }
  }
}
