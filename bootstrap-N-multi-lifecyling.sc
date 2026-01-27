// bootstrap-N-multi-lifecycling.sc (Canton OSS 3.4.10 compatible)
//
// Goal: enable multi-submit from participant2 (ledger API port 6965)
// by multi-hosting required parties onto participant2 with Submission permission.
//
// participant1: alice, aliceBank
// participant2: bob, bobBank      (SCRIPT RUNNER targets this participant)
// participant3: centralBank
//
// Key fixes vs previous version:
//  - wait for EFFECTIVE mappings (proposals=false) instead of proposals=true
//  - propose deltas ONLY from the party's "owning" participant (p1 for alice, p3 for centralBank)
//    to avoid stuck proposals
//  - sanity-checks are done on effective head state
//

import com.digitalasset.canton.config.RequireTypes
import com.digitalasset.canton.protocol.ProtocolVersion
import com.digitalasset.canton.topology.{ParticipantPermission, PartyId}
import com.digitalasset.canton.topology.admin.grpc.TopologyStoreId.Synchronizer
import com.digitalasset.canton.time.TimeQuery
import com.digitalasset.canton.topology.admin.grpc.StaticSynchronizerParameters

// ----------------------------------------------------------------------------
// Create synchronizer
bootstrap.synchronizer(
  synchronizerName = "mysynchronizer",
  sequencers = Seq(sequencer1),
  mediators = Seq(mediator1),
  synchronizerOwners = Seq(sequencer1, mediator1),
  synchronizerThreshold = RequireTypes.PositiveInt.tryCreate(2),
  staticSynchronizerParameters =
    StaticSynchronizerParameters.defaultsWithoutKMS(
      ProtocolVersion.tryCreate("dev")
    )
)

// ----------------------------------------------------------------------------
// Avoid NOT_SERVING race on sequencer public API
println("[bootstrap] Waiting a bit for sequencer public API to become ready...")
Thread.sleep(8000)

// Connect participants
participant1.synchronizers.connect_local(sequencer1, "mysynchronizer")
participant2.synchronizers.connect_local(sequencer1, "mysynchronizer")
participant3.synchronizers.connect_local(sequencer1, "mysynchronizer")

// Force reconnect
participant1.synchronizers.reconnect_local("mysynchronizer", true)
participant2.synchronizers.reconnect_local("mysynchronizer", true)
participant3.synchronizers.reconnect_local("mysynchronizer", true)

// Wait until active
def waitActive(alias: String, attempts: Int = 60): Unit = {
  var i = 0
  while (
    i < attempts &&
    (!participant1.synchronizers.active(alias)
      || !participant2.synchronizers.active(alias)
      || !participant3.synchronizers.active(alias))
  ) {
    println(s"[bootstrap] Waiting for participants to become active on $alias ...")
    Thread.sleep(1000)
    i += 1
  }
  require(participant1.synchronizers.active(alias), s"participant1 is still not active on $alias")
  require(participant2.synchronizers.active(alias), s"participant2 is still not active on $alias")
  require(participant3.synchronizers.active(alias), s"participant3 is still not active on $alias")
}

waitActive("mysynchronizer")
println("[bootstrap] All three participants are active on mysynchronizer.")

// Useful store ids (synchronizer ids)
val sid1 = participant1.synchronizers.id_of("mysynchronizer")
val sid2 = participant2.synchronizers.id_of("mysynchronizer")
val sid3 = participant3.synchronizers.id_of("mysynchronizer")

// ----------------------------------------------------------------------------
// Enable parties (after synchronizer is active)
val alicePartyId        = participant1.parties.enable("alice")
val aliceBankPartyId    = participant1.parties.enable("aliceBank")

val bobPartyId          = participant2.parties.enable("bob")
val bobBankPartyId      = participant2.parties.enable("bobBank")

val centralBankPartyId  = participant3.parties.enable("centralBank")

println(s"[bootstrap] alice FULL:        ${alicePartyId.toProtoPrimitive}")
println(s"[bootstrap] aliceBank FULL:    ${aliceBankPartyId.toProtoPrimitive}")
println(s"[bootstrap] bob FULL:          ${bobPartyId.toProtoPrimitive}")
println(s"[bootstrap] bobBank FULL:      ${bobBankPartyId.toProtoPrimitive}")
println(s"[bootstrap] centralBank FULL:  ${centralBankPartyId.toProtoPrimitive}")

// ----------------------------------------------------------------------------
// Upload DAR to all participants
val dar =
  sys.env.getOrElse(
    "DAR",
    "/Users/karenstaner/daml-finance/package/test/daml/Daml.Finance.Benchmark.Test/.daml/dist/daml-finance-benchmark-test-0.99.0.20251211.0.dar"
  )

println(s"[bootstrap] Using DAR: $dar")

val before = participant1.packages.list().map(_.packageId).toSet

val p1Main = participant1.dars.upload(dar)
val p2Main = participant2.dars.upload(dar)
val p3Main = participant3.dars.upload(dar)

println(s"[bootstrap] participant1 mainPackageId: $p1Main")
println(s"[bootstrap] participant2 mainPackageId: $p2Main")
println(s"[bootstrap] participant3 mainPackageId: $p3Main")

val after = participant1.packages.list().map(_.packageId).toSet
val newPkgIds = (after -- before).toSeq.sorted
println(s"[bootstrap] New packageIds: ${newPkgIds.mkString(", ")}")

// Check vetting (read from participant1's topology view; it includes entries for all participants)
val v = participant1.topology.vetted_packages.list()

val p1VettedPkgIdStrings =
  v.find(_.item.participantId == participant1.id).toSeq
    .flatMap(_.item.packages.map(_.packageId.toString)).toSet

val p2VettedPkgIdStrings =
  v.find(_.item.participantId == participant2.id).toSeq
    .flatMap(_.item.packages.map(_.packageId.toString)).toSet

val p3VettedPkgIdStrings =
  v.find(_.item.participantId == participant3.id).toSeq
    .flatMap(_.item.packages.map(_.packageId.toString)).toSet

val p1Missing = newPkgIds.filterNot(p1VettedPkgIdStrings.contains)
val p2Missing = newPkgIds.filterNot(p2VettedPkgIdStrings.contains)
val p3Missing = newPkgIds.filterNot(p3VettedPkgIdStrings.contains)

println(s"[bootstrap] Missing vetted packages for participant1: ${p1Missing.mkString(", ")}")
println(s"[bootstrap] Missing vetted packages for participant2: ${p2Missing.mkString(", ")}")
println(s"[bootstrap] Missing vetted packages for participant3: ${p3Missing.mkString(", ")}")

require(p1Missing.isEmpty, "participant1 is missing vetted packages: " + p1Missing.mkString(", "))
require(p2Missing.isEmpty, "participant2 is missing vetted packages: " + p2Missing.mkString(", "))
require(p3Missing.isEmpty, "participant3 is missing vetted packages: " + p3Missing.mkString(", "))

println("[bootstrap] DAR uploaded and packages are vetted for all participants.")

// ----------------------------------------------------------------------------
// Helpers: wait + print EFFECTIVE party mapping on participant2
def listPartyMappingOnP2(fullParty: PartyId, proposals: Boolean): Seq[com.digitalasset.canton.admin.api.client.data.topology.ListPartyToParticipantResult] =
  participant2.topology.party_to_participant_mappings.list(
    synchronizerId = sid2,
    proposals = proposals,
    timeQuery = TimeQuery.HeadState,
    operation = None,
    filterParty = fullParty.toProtoPrimitive,
    filterParticipant = "",
    filterSigningKey = "",
    protocolVersion = None
  )

def waitPartyEffectiveOnP2(fullParty: PartyId, attempts: Int = 60): Unit = {
  var i = 0
  while (i < attempts) {
    val rs = listPartyMappingOnP2(fullParty, proposals = false)

    val ok =
      rs.exists(r =>
        r.item.partyId.toProtoPrimitive == fullParty.toProtoPrimitive &&
        r.item.participants.exists(hp =>
          hp.participantId.toString == participant2.id.toString &&
          hp.permission == ParticipantPermission.Submission
        )
      )

    if (ok) {
      println(s"[bootstrap-multi] EFFECTIVE on p2: ${fullParty.toProtoPrimitive}")
      return
    }

    println(s"[bootstrap-multi] Waiting for EFFECTIVE hosting on p2: ${fullParty.toProtoPrimitive} ...")
    Thread.sleep(1000)
    i += 1
  }

  // Helpful debug on timeout
  val props = listPartyMappingOnP2(fullParty, proposals = true)
  println(s"[bootstrap-multi] TIMEOUT. proposals=true entries for ${fullParty.toProtoPrimitive}: ${props.size}")
  props.foreach(println)

  val eff = listPartyMappingOnP2(fullParty, proposals = false)
  println(s"[bootstrap-multi] TIMEOUT. proposals=false entries for ${fullParty.toProtoPrimitive}: ${eff.size}")
  eff.foreach(println)

  sys.error(s"Timed out waiting for EFFECTIVE hosting on p2: ${fullParty.toProtoPrimitive}")
}

def printPartyMappingEffectiveOnP2(fullParty: PartyId): Unit = {
  val res = listPartyMappingOnP2(fullParty, proposals = false)
  if (res.isEmpty) {
    println(s"[bootstrap-multi] NO effective mapping found on p2 for party=${fullParty.toProtoPrimitive}")
    return
  }

  res.foreach { r =>
    val partyFull = r.item.partyId.toProtoPrimitive
    val hosts = r.item.participants.map { hp =>
      s"${hp.participantId.toString}:${hp.permission}"
    }.mkString(", ")
    println(s"[bootstrap-multi] Effective mapping on p2: partyFull=$partyFull hosts=[$hosts]")
  }
}

// ----------------------------------------------------------------------------
// Multi-host onto participant2 (required for multi-submit)
// IMPORTANT: propose ONLY from the party's original host participant.
// ----------------------------------------------------------------------------

// centralBank: enabled on participant3 -> add participant2 as Submission host
println("[bootstrap-multi] Multi-hosting centralBank onto participant2 (propose from participant3)...")
participant3.topology.party_to_participant_mappings.propose_delta(
  party = centralBankPartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid3
)
waitPartyEffectiveOnP2(centralBankPartyId)
printPartyMappingEffectiveOnP2(centralBankPartyId)

// alice: enabled on participant1 -> add participant2 as Submission host
println("[bootstrap-multi] Multi-hosting alice onto participant2 (propose from participant1)...")
participant1.topology.party_to_participant_mappings.propose_delta(
  party = alicePartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid1
)
waitPartyEffectiveOnP2(alicePartyId)
printPartyMappingEffectiveOnP2(alicePartyId)

// bob: enabled on participant2, should already be effective
println("[bootstrap-multi] Sanity-checking bob is hosted on participant2...")
waitPartyEffectiveOnP2(bobPartyId)
printPartyMappingEffectiveOnP2(bobPartyId)

println("[bootstrap] Done: synchronizer up, parties enabled, DAR vetted, and required parties EFFECTIVE on participant2.")
