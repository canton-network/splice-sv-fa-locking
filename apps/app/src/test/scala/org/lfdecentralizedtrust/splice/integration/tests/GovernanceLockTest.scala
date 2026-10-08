package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.data.CantonTimestamp
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.api.token.transferinstructionv1
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.console.ParticipantClientReference
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.SpliceTestConsoleEnvironment
import org.lfdecentralizedtrust.tokenstandard.transferinstruction

import scala.jdk.OptionConverters.*

trait GovernanceLockTest extends TokenStandardTest {
  private val lockCipPrefix = "cip-127"
  private val lockMemoPrefix = s"$lockCipPrefix/memo:"

  val lockManagerParty = PartyId.tryFromProtoPrimitive(
    s"${lockCipPrefix}_lock-manager::1220000000000000000000000000000000000000000000000000000000000000abcd"
  )

  sealed abstract class LockRequest[A](
      val request: String,
      val memoEntries: A => List[(String, String)],
  )
  case object CreateLock extends LockRequest[Unit]("create-lock", _ => Nil)
  case object UnlockAndStartVesting
      extends LockRequest[(BigDecimal, Option[CantonTimestamp])](
        "unlock-and-start-vesting",
        { case (unlockAmount, unlockAt) =>
          ("unlock-amount", unlockAmount.toString) ::
            unlockAt.map(t => ("unlock-at", t.toMicros.toString)).toList
        },
      )

  sealed abstract class LockKind(val kind: String)
  case object SuperValidatorLock extends LockKind("sv-lock")
  case object ProvisionalFeaturedAppLock extends LockKind("provisional-fa-lock")

  sealed abstract class LockStatus(val status: String)
  case object Locked extends LockStatus("locked")
  case object Vesting extends LockStatus("vesting")

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
        Map("lock-status" -> lockStatus.status)
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

  def compareLockMemo(actual: String, expected: String) = {
    val actualEntries = extractLockMemoEntries(actual)
    val expectedEntries = extractLockMemoEntries(expected)

    actualEntries shouldBe expectedEntries withClue s"actual: $actual, expected: $expected"
  }

  val governanceLockUnlockAtMetaKey = s"$lockCipPrefix/unlock-at"
  val vestingLockWithdrawAtMetaKey = s"$lockCipPrefix/withdraw-at"

  def createGovernanceLockViaTokenStandard(
      participant: ParticipantClientReference,
      owner: RichPartyId,
      lockKind: LockKind,
      lockSubject: String,
      amount: BigDecimal,
  )(implicit
      env: SpliceTestConsoleEnvironment
  ): transferinstructionv1.TransferInstruction.ContractId = {
    // `owner` may already have pending instructions to `lockManagerParty` (earlier
    // `GovernanceLock`s or `VestingLock`s), so only look at the new one.
    val existingInstructionCids =
      listTransferInstructions(participant, owner.partyId).map(_._1).toSet
    executeTransferViaTokenStandard(
      participant,
      owner,
      lockManagerParty,
      amount,
      transferinstruction.v1.definitions.TransferFactoryWithChoiceContext.TransferKind.Offer,
      description = Some(makeInputLockMemo(CreateLock, lockKind, lockSubject)(())),
    )
    listTransferInstructions(participant, owner.partyId).collect {
      case (cid, view)
          if view.transfer.receiver == lockManagerParty.toProtoPrimitive &&
            !existingInstructionCids.contains(cid) =>
        cid
    }.loneElement
  }

  /** Unlocks a governance lock by submitting an unlock request */
  def unlockGovernanceLockViaTokenStandard(
      participant: ParticipantClientReference,
      owner: RichPartyId,
      lockKind: LockKind,
      lockSubject: String,
      governanceLockCid: transferinstructionv1.TransferInstruction.ContractId,
      unlockAmount: BigDecimal,
      unlockAt: Option[CantonTimestamp] = None,
  )(implicit env: SpliceTestConsoleEnvironment): Option[
    (
        transferinstructionv1.TransferInstruction.ContractId,
        transferinstructionv1.TransferInstructionView,
    )
  ] =
    actAndCheck(
      "the owner unlocks the GovernanceLock",
      executeTransferViaTokenStandard(
        participant,
        owner,
        lockManagerParty,
        0,
        transferinstruction.v1.definitions.TransferFactoryWithChoiceContext.TransferKind.Offer,
        description = Some(
          makeInputLockMemo(UnlockAndStartVesting, lockKind, lockSubject)((unlockAmount, unlockAt))
        ),
        wait = false,
      ),
    )(
      "a VestingLock is not the pending TransferInstruction",
      _ =>
        listTransferInstructions(participant, owner.partyId).collect {
          case t @ (_, view)
              if view.originalInstructionCid.toScala.exists(
                _.contractId == governanceLockCid.contractId
              ) =>
            t
        } match {
          case Seq() => None
          case Seq(vestingLockCid) => Some(vestingLockCid)
          case many => fail(s"Expected at most one VestingLock for $governanceLockCid, got $many")
        },
    )._2
}
