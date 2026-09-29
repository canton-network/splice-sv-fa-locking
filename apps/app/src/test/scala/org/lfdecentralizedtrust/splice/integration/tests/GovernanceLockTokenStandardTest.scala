package org.lfdecentralizedtrust.splice.integration.tests

import com.digitalasset.canton.data.CantonTimestamp
import com.digitalasset.canton.topology.PartyId
import org.lfdecentralizedtrust.splice.codegen.java.splice.api.token.{
  allocationv2,
  metadatav1,
  transferinstructionv1,
}
import org.lfdecentralizedtrust.splice.console.LedgerApiExtensions.RichPartyId
import org.lfdecentralizedtrust.splice.console.ParticipantClientReference
import org.lfdecentralizedtrust.splice.integration.tests.SpliceTests.SpliceTestConsoleEnvironment
import org.lfdecentralizedtrust.splice.util.ChoiceContextWithDisclosures
import org.lfdecentralizedtrust.tokenstandard.transferinstruction

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait GovernanceLockTokenStandardTest extends TokenStandardTest {
  private val cipPrefix = "cip-0105"

  private def makeLockType(type_ : String): String = s"${cipPrefix}_$type_"

  val superValidatorLockType = makeLockType("sv-lock")
  val featuredAppLockType = makeLockType("fa-lock")
  val provisionalFeaturedAppLockType = makeLockType("provisional-fa-lock")

  private val lockMagicPartyNamespace =
    "::1220000000000000000000000000000000000000000000000000000000000000abcd"

  private def makeLockMagicParty(type_ : String): PartyId =
    PartyId.tryFromProtoPrimitive(type_ + lockMagicPartyNamespace)

  val superValidatorLockMagicParty = makeLockMagicParty(superValidatorLockType)
  val featuredAppLockMagicParty = makeLockMagicParty(featuredAppLockType)
  val provisionalFeaturedAppLockMagicParty = makeLockMagicParty(provisionalFeaturedAppLockType)

  def lockTypeForMagicParty(lockParty: PartyId): String =
    lockParty.toProtoPrimitive.stripSuffix(lockMagicPartyNamespace)

  private def makeMetaKey(key: String): String = s"$cipPrefix/$key"

  val lockSubjectMetaKey = makeMetaKey("lock-subject")
  val governanceLockUnlockAtMetaKey = makeMetaKey("unlock-at")
  val vestingLockWithdrawAtMetaKey = makeMetaKey("withdraw-at")

  def makeLockSubject(lockSubject: String): String = s"lock-subject=$lockSubject"

  sealed abstract class GovernanceLockTokenStandardOps[CId, View](val version: Int) {
    def create(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        lockParty: PartyId,
        lockSubject: String,
        amount: BigDecimal,
    )(implicit env: SpliceTestConsoleEnvironment): Unit

    def list(participantClient: ParticipantClientReference, party: PartyId): Seq[(CId, View)]

    def getWithdrawContext(cid: CId)(implicit
        env: SpliceTestConsoleEnvironment
    ): ChoiceContextWithDisclosures

    final def createGovernanceLock(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        lockParty: PartyId,
        lockSubject: String,
        amount: BigDecimal,
    )(implicit env: SpliceTestConsoleEnvironment): CId = {
      val existing = list(participant, owner.partyId).map(_._1).toSet
      create(participant, owner, lockParty, lockSubject, amount)
      list(participant, owner.partyId).collect {
        case (cid, view) if !existing.contains(cid) => cid
      }.loneElement
    }

    def withdraw(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        cid: CId,
        meta: Map[String, String],
    )(implicit env: SpliceTestConsoleEnvironment): Unit

    private def withdrawAndList(
        clue: String,
        participant: ParticipantClientReference,
        owner: RichPartyId,
        cid: CId,
        meta: Map[String, String],
        isValid: View => Boolean = _ => true,
    )(implicit env: SpliceTestConsoleEnvironment): Option[(CId, View)] =
      actAndCheck(clue, withdraw(participant, owner, cid, meta))(
        "at most one new VestingLock is created",
        _ =>
          list(participant, owner.partyId).filter(t => isValid(t._2)) match {
            case Seq() => None
            case Seq(lock) => Some(lock)
            case many => fail(s"Expected at most one VestingLock, got $many")
          },
      )._2

    final def unlockGovernanceLock(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        cid: CId,
        meta: Map[String, String] = Map.empty,
        isValid: View => Boolean = _ => true,
    )(implicit env: SpliceTestConsoleEnvironment): Option[(CId, View)] =
      withdrawAndList(
        "the owner unlocks the GovernanceLock",
        participant,
        owner,
        cid,
        meta,
        isValid,
      )

    final def withdrawVestingLock(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        cid: CId,
        meta: Map[String, String] = Map.empty,
    )(implicit env: SpliceTestConsoleEnvironment): Option[(CId, View)] =
      withdrawAndList("the owner withdraws the VestingLock", participant, owner, cid, meta)
  }

  object tsv1Operations
      extends GovernanceLockTokenStandardOps[
        transferinstructionv1.TransferInstruction.ContractId,
        transferinstructionv1.TransferInstructionView,
      ](1) {
    def create(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        lockParty: PartyId,
        lockSubject: String,
        amount: BigDecimal,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      executeTransferViaTokenStandard(
        participant,
        owner,
        lockParty,
        amount,
        transferinstruction.v1.definitions.TransferFactoryWithChoiceContext.TransferKind.Offer,
        description = Some(makeLockSubject(lockSubject)),
      )

    def list(participantClient: ParticipantClientReference, party: PartyId) =
      listTransferInstructions(participantClient, party)

    def getWithdrawContext(
        cid: transferinstructionv1.TransferInstruction.ContractId
    )(implicit env: SpliceTestConsoleEnvironment): ChoiceContextWithDisclosures =
      sv1ScanBackend.getTransferInstructionWithdrawContext(cid)

    def withdraw(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        cid: transferinstructionv1.TransferInstruction.ContractId,
        meta: Map[String, String],
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      withdrawTransferInstruction(participant, owner, cid, meta = meta)
  }

  object tsv2Operations
      extends GovernanceLockTokenStandardOps[
        allocationv2.Allocation.ContractId,
        allocationv2.AllocationView,
      ](2) {
    def create(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        lockParty: PartyId,
        lockSubject: String,
        amount: BigDecimal,
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      executeAllocationViaTokenStandardV2(
        participant,
        owner,
        new allocationv2.SettlementInfo(
          List(dsoParty.toProtoPrimitive).asJava,
          lockTypeForMagicParty(lockParty),
          None.toJava,
          new metadatav1.Metadata(Map.empty.asJava),
        ),
        transferLegSides = Seq.empty,
        settlementDeadline = Some(CantonTimestamp.MaxValue.addMicros(-1).toInstant),
        nextIterationFunding = Some(Map("Amulet" -> amount)),
        committed = false,
        allocationMeta = Map(lockSubjectMetaKey -> lockSubject),
        extraArgsMeta = Map.empty,
      )

    def list(participantClient: ParticipantClientReference, party: PartyId) =
      listAllocationsV2(participantClient, party)

    def getWithdrawContext(
        cid: allocationv2.Allocation.ContractId
    )(implicit env: SpliceTestConsoleEnvironment): ChoiceContextWithDisclosures =
      sv1ScanBackend.getAllocationV2WithdrawContext(cid)

    def withdraw(
        participant: ParticipantClientReference,
        owner: RichPartyId,
        cid: allocationv2.Allocation.ContractId,
        meta: Map[String, String],
    )(implicit env: SpliceTestConsoleEnvironment): Unit =
      withdrawAllocationV2(participant, owner, cid, meta = meta)
  }
}
