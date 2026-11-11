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
  ValidatorAppReference,
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

  private val lockStatusMemoKey = "lock-status"
  private val lockVestingDurationMemoKey = "lock-vesting-duration-micros"
  private val lockVestingEndTimeMemoKey = "lock-vesting-end-time-micros"
  private val targetLockOwnerMemoKey = "target-lock-owner"

  protected val tiaAccept = new tiv2.transferinstructionaction.TIA_Accept(DamlUnit.getInstance)
  protected val tiaReject = new tiv2.transferinstructionaction.TIA_Reject(DamlUnit.getInstance)
  protected val tiaWithdraw = new tiv2.transferinstructionaction.TIA_Withdraw(DamlUnit.getInstance)

  private def durationToRelTime(d: Duration): RelTime =
    new RelTime(TimeUnit.NANOSECONDS.toMicros(d.toNanos))

  private def instantToMicros(i: Instant): Long = ChronoUnit.MICROS.between(Instant.EPOCH, i)

  private def formatInstant(i: Instant): String =
    DateTimeFormatter.ISO_INSTANT.format(i.truncatedTo(ChronoUnit.MICROS))

  sealed abstract class LockKind(val kind: String)
  case object SuperValidatorLock extends LockKind("sv-lock")
  case object ProvisionalFeaturedAppLock extends LockKind("provisional-fa-lock")

  sealed abstract class LockStatus(val status: String)
  case object Locked extends LockStatus("locked")
  case object Vesting extends LockStatus("vesting")

  case class LockControllers(
      unlockControllers: List[List[PartyId]] = Nil,
      substitutionControllers: List[List[PartyId]] = Nil,
      vestingControllers: List[List[PartyId]] = Nil,
  ) {
    final def memoEntriesWithPrefix(prefix: String) =
      List(
        (prefix ++ "unlock-controllers") -> unlockControllers,
        (prefix ++ "substitution-controllers") -> substitutionControllers,
        (prefix ++ "vesting-controllers") -> vestingControllers,
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
  case object CreateLock extends LockRequest[LockControllers]("create-lock", _.memoEntries)
  case object UnlockAndStartVesting
      extends LockRequest[(BigDecimal, Instant, Option[PartyId], LockControllers)](
        "unlock-and-start-vesting",
        { case (unlockAmount, vestingStartTime, targetLockOwner, controllers) =>
          List(
            "unlock-amount" -> unlockAmount.toString,
            "vesting-start-time" -> formatInstant(vestingStartTime),
          ) ++
            targetLockOwner.map(p => (targetLockOwnerMemoKey, p.toProtoPrimitive)) ++
            controllers.memoEntries
        },
      )
  case object WithdrawVestedFunds
      extends LockRequest[(Instant, RelTime, Instant, Option[PartyId], LockControllers)](
        "withdraw-vested-funds",
        { case (vestedUntilTime, vestingPeriod, vestingEndTime, targetLockOwner, controllers) =>
          List(
            "vested-until-time" -> formatInstant(vestedUntilTime),
            lockVestingDurationMemoKey -> vestingPeriod.microseconds.toString,
            lockVestingEndTimeMemoKey -> instantToMicros(vestingEndTime).toString,
          ) ++
            targetLockOwner.map(p => (targetLockOwnerMemoKey, p.toProtoPrimitive)) ++
            controllers.memoEntries
        },
      )

  case object SubstituteLock
      extends LockRequest[
        (LockStatus, PartyId, Option[RelTime], Option[Instant], LockControllers, LockControllers)
      ](
        "substitute-lock",
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
              lockStatusMemoKey -> lockStatus.status,
              targetLockOwnerMemoKey -> targetLockOwner.toProtoPrimitive,
            ) ++
              vestingPeriod.map(t => (lockVestingDurationMemoKey, t.microseconds.toString)) ++
              vestingEndTime.map(t => (lockVestingEndTimeMemoKey, instantToMicros(t).toString)) ++
              targetControllers.memoEntries ++
              newControllers.memoEntriesWithPrefix("new-")
        },
      )

  case object TransferLock
      extends LockRequest[
        (
            LockStatus,
            PartyId,
            BigDecimal,
            Option[RelTime],
            Option[Instant],
            LockControllers,
            LockControllers,
        )
      ](
        "transfer-lock",
        {
          case (
                lockStatus,
                newLockOwner,
                transferAmount,
                vestingPeriod,
                vestingEndTime,
                sourceControllers,
                newControllers,
              ) =>
            List(
              lockStatusMemoKey -> lockStatus.status,
              "new-lock-owner" -> newLockOwner.toProtoPrimitive,
              "transfer-amount" -> transferAmount.toString,
            ) ++
              vestingPeriod.map(t => (lockVestingDurationMemoKey, t.microseconds.toString)) ++
              vestingEndTime.map(t => (lockVestingEndTimeMemoKey, instantToMicros(t).toString)) ++
              sourceControllers.memoEntries ++
              newControllers.memoEntriesWithPrefix("new-")
        },
      )

  private def makeLockMemo(entries: List[(String, String)]): String =
    lockMemoPrefix + entries.map { case (k, v) => s"$k=$v" }.mkString("&")

  private def commonLockMemoEntries(
      lockKind: LockKind,
      lockSubject: String,
  ): List[(String, String)] = List("lock-kind" -> lockKind.kind, "lock-subject" -> lockSubject)

  def makeInputLockMemo[A](
      lockRequest: LockRequest[A],
      lockKind: LockKind,
      lockSubject: String,
  )(data: A): String =
    makeLockMemo(
      List("request" -> lockRequest.request) ++
        commonLockMemoEntries(lockKind, lockSubject) ++
        lockRequest.memoEntries(data)
    )

  def makeOutputLockMemo(
      lockKind: LockKind,
      lockSubject: String,
      lockStatus: LockStatus,
  ): String =
    makeLockMemo(
      commonLockMemoEntries(lockKind, lockSubject) ++
        Map(lockStatusMemoKey -> lockStatus.status)
    )

  /** The memo of a pending transfer of a lock that is not vesting */
  def makeAcceptTransferLockMemo(lockKind: LockKind, lockSubject: String): String =
    makeLockMemo(
      List("request" -> "accept-to-transfer-lock") ++
        commonLockMemoEntries(lockKind, lockSubject) ++
        Map(lockStatusMemoKey -> Locked.status)
    )

  // Using a Map so order doesn't matter when comparing below
  private def extractLockMemoEntries(memo: String): Map[String, String] =
    memo
      .stripPrefix(lockMemoPrefix)
      .split("&")
      .flatMap(_.split("=", 2) match {
        case Array(k, v) => Some((k, v))
        case _ => None
      })
      .toMap

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
    def pendingActions(transfer: View): Map[tiv2.TransferInstructionAction, List[String]]
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
                  case a if a.contains("accept") => Map(tiaAccept -> List(party))
                  case a if a.contains("reject") => Map(tiaReject -> List(party))
                  case a if a.contains("withdraw") => Map(tiaWithdraw -> List(party))
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
            (action, parties.asScala.toList.flatMap(_.asScala.toList).distinct)
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
        lockKind: LockKind,
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
        lockKind: LockKind,
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
        lockKind: LockKind,
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
        lockStatus: LockStatus,
        targetLockOwner: PartyId,
        vestingPeriod: Option[RelTime],
        vestingEndTime: Option[Instant],
    ) {
      final def apply(
          participant: ParticipantClientReference,
          proposer: RichPartyId,
          lockKind: LockKind,
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
      new SubstituteLock(Locked, owner, None, None)

    /** Propose a substitution of a GovernanceLock */
    def substituteGovernanceLock(lock: gl.GovernanceLock): SubstituteLock =
      substituteGovernanceLock(PartyId.tryFromProtoPrimitive(lock.owner))

    /** Propose a substitution of a vesting GovernanceLock with the given attributes */
    def substituteVestingLock(owner: PartyId, vestingPeriod: Duration, vestingEndTime: Instant) =
      new SubstituteLock(
        Vesting,
        owner,
        Some(durationToRelTime(vestingPeriod)),
        Some(vestingEndTime),
      )

    /** Propose a substitution of a vesting GovernanceLock */
    def substituteVestingLock(lock: gl.GovernanceLock) =
      new SubstituteLock(
        Vesting,
        PartyId.tryFromProtoPrimitive(lock.owner),
        Some(vestingLockSpecification(lock).vestingPeriod),
        Some(vestingLockSpecification(lock).endTime),
      )

    final class TransferLock(
        lockStatus: LockStatus,
        vestingPeriod: Option[RelTime],
        vestingEndTime: Option[Instant],
    ) {
      final def apply(
          participant: ParticipantClientReference,
          owner: RichPartyId,
          lockKind: LockKind,
          lockSubject: String,
          newLockOwner: PartyId,
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
    final val transferGovernanceLock = new TransferLock(Locked, None, None)

    /** Propose a transfer of a vesting GovernanceLock */
    final def transferVestingLock(vestingPeriod: Duration, vestingEndTime: Instant) =
      new TransferLock(Vesting, Some(durationToRelTime(vestingPeriod)), Some(vestingEndTime))

    /** Propose a transfer of part of a vesting GovernanceLock, keeping its
      * vesting schedule
      */
    final def transferVestingLock(lock: gl.GovernanceLock) =
      new TransferLock(
        Vesting,
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
    case class Parties(
        alice: RichPartyId,
        bob: RichPartyId,
        charlie: RichPartyId,
        sv1: RichPartyId,
    ) {
      final val all = List(alice, bob, charlie, sv1)
    }

    private def makeParty(
        walletAppClient: WalletAppClientReference,
        validator: ValidatorAppReference,
    ): RichPartyId = RichPartyId.local(onboardWalletUser(walletAppClient, validator))

    type Propose[CId, View] = (
        Parties,
        ParticipantClientReference,
        NonEmptyList[RichPartyId],
        RichPartyId,
        LockControllers,
    ) => (CId, View)

    private def setupLockAndProposal[
        ICId <: ContractId[?],
        IView,
        ACId,
        TCId <: ContractId[T],
        TC <: Contract[TCId, T],
        T <: Template,
    ](
        ops: TokenStandardOps[ICId, IView],
        proposalCompanion: ContractCompanion[TC, TCId, T],
    )(
        makeControllers: List[List[PartyId]] => LockControllers,
        setupLock: (RichPartyId, LockControllers) => gl.GovernanceLock.ContractId,
        makeProposers: Parties => NonEmptyList[RichPartyId],
        proposalValidator: ValidatorAppBackendReference,
        propose: Propose[ICId, IView],
        expectedAmount: BigDecimal,
        expectedReceiver: (Parties, RichPartyId) => PartyId,
    )(implicit env: SpliceTestConsoleEnvironment) = {
      val parties = Parties(
        alice = makeParty(aliceWalletClient, aliceValidatorBackend),
        bob = makeParty(bobWalletClient, bobValidatorBackend),
        charlie = makeParty(charlieWalletClient, aliceValidatorBackend),
        sv1 = makeParty(sv1WalletClient, sv1ValidatorBackend),
      )
      val controllers = makeControllers(
        List(
          List(parties.alice, parties.bob, parties.charlie),
          List(parties.bob, parties.charlie, parties.sv1),
        ).map(_.map(_.partyId))
      )
      val proposers0 = makeProposers(parties)
      val proposers = ops match {
        // TSv1 can only have one proposer
        case `tsv1Operations` => NonEmptyList.one(proposers0.head)
        case `tsv2Operations` => proposers0.sortBy(_.partyId.toProtoPrimitive)
      }
      val proposersSet = proposers.toList.toSet
      val nonProposers = parties.all.filterNot(proposersSet.contains)
      val lockCid = setupLock(parties.alice, controllers)

      // the proposers initiate the proposal
      val (proposalCid, proposalView) = propose(
        parties,
        proposalValidator.participantClientWithAdminToken,
        proposers,
        parties.alice,
        controllers,
      )
      // Prove that proposalCid is the correct proposal type
      val _ = proposalValidator.participantClientWithAdminToken.ledger_api_extensions.acs
        .filterJava(proposalCompanion)(
          parties.bob.partyId,
          predicate = _.id.contractId == proposalCid.contractId,
        )
        .loneElement

      ops.transferValues.sender(proposalView) shouldBe proposers.head.partyId.toProtoPrimitive
      ops.transferValues.receiver(proposalView) shouldBe
        expectedReceiver(parties, parties.alice).toProtoPrimitive
      ops.transferValues.amount(proposalView) shouldBe expectedAmount
      ops.transferValues.pendingActions(proposalView) shouldBe
        Map(
          tiaAccept -> nonProposers.map(_.partyId.toProtoPrimitive),
          tiaReject -> nonProposers.map(_.partyId.toProtoPrimitive),
          tiaWithdraw -> proposers.toList.map(_.partyId.toProtoPrimitive),
        )

      (parties, lockCid, proposalCid)
    }

    def testProposalAccept[
        ICId <: ContractId[?],
        IView,
        ACId,
        TCId <: ContractId[T],
        TC <: Contract[TCId, T],
        T <: Template,
    ](
        ops: TokenStandardOps[ICId, IView],
        proposalCompanion: ContractCompanion[TC, TCId, T],
    )(
        makeControllers: List[List[PartyId]] => LockControllers,
        setupLock: (RichPartyId, LockControllers) => gl.GovernanceLock.ContractId,
        makeProposers: Parties => NonEmptyList[RichPartyId],
        proposalValidator: ValidatorAppBackendReference,
        propose: Propose[ICId, IView],
        expectedAmount: BigDecimal,
        expectedReceiver: (Parties, RichPartyId) => PartyId,
        checkUpdatedLocks: (Parties, Seq[(ICId, IView, gl.GovernanceLock.Contract)]) => Unit,
    )(implicit env: SpliceTestConsoleEnvironment): Seq[gl.GovernanceLock.Contract] = {
      // bob proposes the proposal
      val (parties, _, proposalCid) = setupLockAndProposal(ops, proposalCompanion)(
        makeControllers,
        setupLock,
        makeProposers,
        proposalValidator,
        propose,
        expectedAmount,
        expectedReceiver,
      )

      clue("Scan serves an accept choice context for the proposal") {
        ops.getAcceptContext(proposalCid).disclosedContracts should not be empty
      }

      // alice accepts the proposal, it remains pending
      tsv2Operations.acceptTransfer(
        aliceValidatorBackend.participantClientWithAdminToken,
        parties.alice,
        tsv2Operations.makeCId(proposalCid.contractId),
      )
      val (stillPendingProposalCid, stillPendingProposalView) = ops
        .listTransfers(bobValidatorBackend.participantClientWithAdminToken, parties.bob.partyId)
        .filter { case (_, view) =>
          ops.transferValues.originalInstructionCid(view) == Some(proposalCid)
        }
        .loneElement
      val stillPendingProposal =
        bobValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
          .filterJava(proposalCompanion)(
            parties.bob.partyId,
            predicate = _.id.contractId == stillPendingProposalCid.contractId,
          )
          .loneElement

      ops.transferValues.sender(stillPendingProposalView) shouldBe
        parties.bob.partyId.toProtoPrimitive
      ops.transferValues.receiver(stillPendingProposalView) shouldBe
        expectedReceiver(parties, parties.alice).toProtoPrimitive
      ops.transferValues.amount(stillPendingProposalView) shouldBe expectedAmount
      ops.transferValues.pendingActions(stillPendingProposalView) shouldBe
        Map(
          tiaAccept -> List(parties.charlie, parties.sv1).map(_.partyId.toProtoPrimitive),
          tiaReject ->
            List(parties.alice, parties.charlie, parties.sv1).map(_.partyId.toProtoPrimitive),
          tiaWithdraw -> List(parties.bob).map(_.partyId.toProtoPrimitive),
        )

      // charlie accepts the proposal, it executes
      tsv2Operations.acceptTransfer(
        aliceValidatorBackend.participantClientWithAdminToken,
        parties.charlie,
        tsv2Operations.makeCId(stillPendingProposal.id.contractId),
      )

      val locks = ops
        .listTransfers(bobValidatorBackend.participantClientWithAdminToken, parties.bob.partyId)
        .map { case (lockCid, lockView) =>
          ops.transferValues.receiver(lockView) shouldBe
            GovernanceLockUtil.lockManagerParty.toProtoPrimitive

          val lock = aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
            .filterJava(gl.GovernanceLock.COMPANION)(
              parties.bob.partyId,
              predicate = _.id.contractId == lockCid.contractId,
            )
            .loneElement

          (lockCid, lockView, lock)
        }

      checkUpdatedLocks(parties, locks)
      locks.map(_._3)
    }

    def testProposalReject[
        ICId <: ContractId[?],
        IView,
        RCId,
        TCId <: ContractId[T],
        TC <: Contract[TCId, T],
        T <: Template,
    ](
        ops: TokenStandardOps[ICId, IView],
        proposalCompanion: ContractCompanion[TC, TCId, T],
    )(
        makeControllers: List[List[PartyId]] => LockControllers,
        setupLock: (RichPartyId, LockControllers) => gl.GovernanceLock.ContractId,
        makeProposers: Parties => NonEmptyList[RichPartyId],
        proposalValidator: ValidatorAppBackendReference,
        propose: Propose[ICId, IView],
        expectedAmount: BigDecimal,
        expectedReceiver: (Parties, RichPartyId) => PartyId,
    )(implicit env: SpliceTestConsoleEnvironment): gl.GovernanceLock.Contract = {
      // bob proposes the proposal
      val (parties, origLockCid, proposalCid) = setupLockAndProposal(ops, proposalCompanion)(
        makeControllers,
        setupLock,
        makeProposers,
        proposalValidator,
        propose,
        expectedAmount,
        expectedReceiver,
      )

      clue("Scan serves a reject choice context for the proposal") {
        ops.getRejectContext(proposalCid).disclosedContracts should not be empty
      }

      // alice rejects the proposal, it remains pending
      tsv2Operations.rejectTransfer(
        aliceValidatorBackend.participantClientWithAdminToken,
        parties.alice,
        tsv2Operations.makeCId(proposalCid.contractId),
      )
      val (stillPendingProposalCid, stillPendingProposalView) = ops
        .listTransfers(bobValidatorBackend.participantClientWithAdminToken, parties.bob.partyId)
        .filter { case (_, view) =>
          ops.transferValues.originalInstructionCid(view) == Some(proposalCid)
        }
        .loneElement
      val stillPendingProposal =
        bobValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
          .filterJava(proposalCompanion)(
            parties.bob.partyId,
            predicate = _.id.contractId == stillPendingProposalCid.contractId,
          )
          .loneElement

      ops.transferValues.sender(stillPendingProposalView) shouldBe
        parties.bob.partyId.toProtoPrimitive
      ops.transferValues.receiver(stillPendingProposalView) shouldBe
        expectedReceiver(parties, parties.alice).toProtoPrimitive
      ops.transferValues.amount(stillPendingProposalView) shouldBe expectedAmount
      ops.transferValues.pendingActions(stillPendingProposalView) shouldBe
        Map(
          tiaAccept ->
            List(parties.alice, parties.charlie, parties.sv1).map(_.partyId.toProtoPrimitive),
          tiaReject -> List(parties.charlie, parties.sv1).map(_.partyId.toProtoPrimitive),
          tiaWithdraw -> List(parties.bob).map(_.partyId.toProtoPrimitive),
        )

      // charlie rejects the proposal, it's archived and the lock is unchanged
      tsv2Operations.rejectTransfer(
        aliceValidatorBackend.participantClientWithAdminToken,
        parties.charlie,
        tsv2Operations.makeCId(stillPendingProposal.id.contractId),
      )
      val (lockCid, lockView) = ops
        .listTransfers(bobValidatorBackend.participantClientWithAdminToken, parties.bob.partyId)
        .loneElement
      val lock = aliceValidatorBackend.participantClientWithAdminToken.ledger_api_extensions.acs
        .filterJava(gl.GovernanceLock.COMPANION)(
          parties.bob.partyId,
          predicate = _.id.contractId == lockCid.contractId,
        )
        .loneElement

      lock.id shouldBe origLockCid
      ops.transferValues.sender(lockView) shouldBe parties.alice.partyId.toProtoPrimitive
      ops.transferValues.receiver(lockView) shouldBe
        GovernanceLockUtil.lockManagerParty.toProtoPrimitive

      lock
    }

    def testProposalWithdraw[
        ICId <: ContractId[?],
        IView,
        TCId <: ContractId[T],
        TC <: Contract[TCId, T],
        T <: Template,
    ](
        ops: TokenStandardOps[ICId, IView],
        proposalCompanion: ContractCompanion[TC, TCId, T],
    )(
        makeControllers: List[List[PartyId]] => LockControllers,
        setupLock: (RichPartyId, LockControllers) => gl.GovernanceLock.ContractId,
        makeProposers: Parties => NonEmptyList[RichPartyId],
        proposalValidator: ValidatorAppBackendReference,
        propose: Propose[ICId, IView],
        expectedAmount: BigDecimal,
        expectedReceiver: (Parties, RichPartyId) => PartyId,
    )(implicit env: SpliceTestConsoleEnvironment): gl.GovernanceLock.Contract = {
      val (parties, origLockCid, proposalCid) = setupLockAndProposal(ops, proposalCompanion)(
        makeControllers,
        setupLock,
        makeProposers,
        proposalValidator,
        propose,
        expectedAmount,
        expectedReceiver,
      )
      val proposers = makeProposers(parties)

      clue("Scan serves a withdraw choice context for the proposal") {
        ops.getWithdrawContext(proposalCid).disclosedContracts should not be empty
      }

      val (stillPendingProposalCid, _): (ICId, List[RichPartyId]) = ops match {
        // TSv1 can only have one proposer, so we can't test that a withdrawal keeps it pending
        case `tsv1Operations` => (proposalCid, Nil)
        case `tsv2Operations` =>
          proposers.tail.foldRight((proposalCid, List.empty[RichPartyId])) {
            case (proposer, (accProposalCid, accProposers)) =>
              // alice withdraws the proposal, it remains pending
              ops.withdrawTransfer(
                proposalValidator.participantClientWithAdminToken,
                proposer,
                accProposalCid,
              )
              val (stillPendingProposalCid, stillPendingProposalView) = ops
                .listTransfers(
                  proposalValidator.participantClientWithAdminToken,
                  proposer.partyId,
                )
                .filter { case (_, view) =>
                  ops.transferValues.originalInstructionCid(view) == Some(proposalCid)
                }
                .loneElement
              val _ =
                proposalValidator.participantClientWithAdminToken.ledger_api_extensions.acs
                  .filterJava(proposalCompanion)(
                    proposer.partyId,
                    predicate = _.id.contractId == stillPendingProposalCid.contractId,
                  )
                  .loneElement

              ops.transferValues.sender(stillPendingProposalView) shouldBe
                parties.alice.partyId.toProtoPrimitive
              ops.transferValues.receiver(stillPendingProposalView) shouldBe
                expectedReceiver(parties, parties.alice).toProtoPrimitive
              ops.transferValues.amount(stillPendingProposalView) shouldBe expectedAmount
              ops.transferValues.pendingActions(stillPendingProposalView) shouldBe
                Map(
                  tiaAccept -> (List(proposer) ++ accProposers ++ List(parties.bob, parties.sv1))
                    .map(_.partyId.toProtoPrimitive),
                  tiaReject -> List(parties.bob, parties.sv1).map(_.partyId.toProtoPrimitive),
                  tiaWithdraw -> List(parties.charlie).map(_.partyId.toProtoPrimitive),
                )

              (stillPendingProposalCid, proposer :: accProposers)
          }
      }

      // the first proposer withdraws the proposal, it's archived and the lock is unchanged
      ops.withdrawTransfer(
        proposalValidator.participantClientWithAdminToken,
        proposers.head,
        stillPendingProposalCid,
      )
      val (lockCid, lockView) = ops
        .listTransfers(proposalValidator.participantClientWithAdminToken, parties.bob.partyId)
        .loneElement
      val lock = proposalValidator.participantClientWithAdminToken.ledger_api_extensions.acs
        .filterJava(gl.GovernanceLock.COMPANION)(
          parties.alice.partyId,
          predicate = _.id.contractId == lockCid.contractId,
        )
        .loneElement

      lock.id shouldBe origLockCid
      ops.transferValues.sender(lockView) shouldBe parties.alice.partyId.toProtoPrimitive
      ops.transferValues.receiver(lockView) shouldBe
        GovernanceLockUtil.lockManagerParty.toProtoPrimitive

      lock
    }

    def testProposal[
        ICId <: ContractId[?],
        IView,
        ARCId,
        TCId <: ContractId[T],
        TC <: Contract[TCId, T],
        T <: Template,
    ](
        ops: TokenStandardOps[ICId, IView],
        proposalCompanion: ContractCompanion[TC, TCId, T],
    )(
        makeControllers: List[List[PartyId]] => LockControllers,
        setupLock: (RichPartyId, LockControllers) => gl.GovernanceLock.ContractId,
        propose: Propose[ICId, IView],
        expectedAmount: BigDecimal,
        expectedReceiver: (Parties, RichPartyId) => PartyId,
        checkUpdatedLocks: (Parties, Seq[(ICId, IView, gl.GovernanceLock.Contract)]) => Unit,
        makeAcceptProposers: Parties => NonEmptyList[RichPartyId] = parties =>
          NonEmptyList.one(parties.bob),
        acceptProposalValidator: Option[ValidatorAppBackendReference] = None,
        makeRejectProposers: Parties => NonEmptyList[RichPartyId] = parties =>
          NonEmptyList.one(parties.bob),
        rejectProposalValidator: Option[ValidatorAppBackendReference] = None,
        makeWithdrawProposers: Parties => NonEmptyList[RichPartyId] = parties =>
          NonEmptyList.of(parties.charlie, parties.alice),
        withdrawProposalValidator: Option[ValidatorAppBackendReference] = None,
    )(implicit env: SpliceTestConsoleEnvironment): Seq[gl.GovernanceLock.Contract] = {
      val origLock = testProposalReject(ops, proposalCompanion)(
        makeControllers,
        setupLock,
        makeRejectProposers,
        rejectProposalValidator.getOrElse(bobValidatorBackend),
        propose,
        expectedAmount,
        expectedReceiver,
      )

      customControllers.testProposalWithdraw(ops, proposalCompanion)(
        makeControllers,
        (_, _) => origLock.id, // lock is already setup by reject above
        makeWithdrawProposers,
        withdrawProposalValidator.getOrElse(aliceValidatorBackend),
        propose,
        expectedAmount,
        expectedReceiver,
      )

      customControllers.testProposalAccept(ops, proposalCompanion)(
        makeControllers,
        (_, _) => origLock.id, // lock is already setup by reject above
        makeAcceptProposers,
        acceptProposalValidator.getOrElse(bobValidatorBackend),
        propose,
        expectedAmount,
        expectedReceiver,
        checkUpdatedLocks,
      )
    }
  }
}
