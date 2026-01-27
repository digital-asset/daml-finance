// bootstrap-N-multi.sc (Canton OSS 3.4.10 compatible)
//
// Enables multi-submit by multi-hosting parties onto participant2.
//
// Goal:
// - run DAML scripts against participant2 ledger (6965) with multi-submit:
//     submit [owner, cp] ...
//     submit [owner, cp, cp2] ...
//
// Therefore participant2 must have Submission permission for:
// - bench-owner (enabled on participant1)
// - bench-counterparty (enabled on participant2, already Submission there)
// - bench-counterparty2 (enabled on participant3)

// ----------------------------------------------------------------------------
// Create synchronizer
//
// NOTE: Canton 3.4.10 does NOT have StaticSynchronizerParameters.defaultsWithoutKMS.
// Use .defaults (then override protocol version if needed).
//
bootstrap.synchronizer(
  synchronizerName = "mysynchronizer",
  sequencers = Seq(sequencer1),
  mediators = Seq(mediator1),
  synchronizerOwners = Seq(sequencer1, mediator1),
  synchronizerThreshold = com.digitalasset.canton.config.RequireTypes.PositiveInt.tryCreate(2),
  staticSynchronizerParameters =
  StaticSynchronizerParameters.defaultsWithoutKMS(
    ProtocolVersion.tryCreate("dev")
  )
)

// ----------------------------------------------------------------------------
// IMPORTANT: Avoid the exact race you are hitting: sequencer public API can be NOT_SERVING
// for a short time after bootstrap. Give it time before connect_local.

println("[bootstrap] Waiting a bit for sequencer public API to become ready...")
Thread.sleep(8000)

// Connect participants
participant1.synchronizers.connect_local(sequencer1, "mysynchronizer")
participant2.synchronizers.connect_local(sequencer1, "mysynchronizer")
participant3.synchronizers.connect_local(sequencer1, "mysynchronizer")

// Force reconnect (Canton 3.4.10 signature: (alias, force:Boolean))
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

// Useful store ids
val sid1 = participant1.synchronizers.id_of("mysynchronizer")
val sid2 = participant2.synchronizers.id_of("mysynchronizer")
val sid3 = participant3.synchronizers.id_of("mysynchronizer")

// ----------------------------------------------------------------------------
// Enable parties (after synchronizer is active)
val ownerPartyId = participant1.parties.enable("bench-owner")
val counterpartyPartyId = participant2.parties.enable("bench-counterparty")
val counterparty2PartyId = participant3.parties.enable("bench-counterparty2")

println(s"[bootstrap] bench-owner partyId FULL:         ${ownerPartyId.toProtoPrimitive}")
println(s"[bootstrap] bench-counterparty partyId FULL:  ${counterpartyPartyId.toProtoPrimitive}")
println(s"[bootstrap] bench-counterparty2 partyId FULL: ${counterparty2PartyId.toProtoPrimitive}")

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

// Check vetting (read from participant1's topology view; it includes entries for both participants)
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

println("[bootstrap] DAR uploaded and packages are vetted for both participants.")

// ----------------------------------------------------------------------------
// Party multi-hosting for multi-submit
// ----------------------------------------------------------------------------
//
// IMPORTANT:
// - bench-owner enabled on participant1 -> add participant2 as Submission host
// - bench-counterparty enabled on participant2 -> already hosted there, do nothing
// - bench-counterparty2 enabled on participant3 -> add participant2 as Submission host
//
// ALSO IMPORTANT: use each participant's own synchronizer id for its own propose_delta.
//
def waitHostedOnP2(partyPrefix: String, attempts: Int = 60): Unit = {
  var i = 0
  while (i < attempts) {
    val auth = participant2.topology.party_to_participant_mappings.list(
      synchronizerId = sid2,
      proposals = false,
      timeQuery = TimeQuery.HeadState,
      operation = None,
      filterParty = partyPrefix,
      filterParticipant = "",
      filterSigningKey = "",
      protocolVersion = None
    )

    val ok =
      auth.exists { r =>
        r.item.participants.exists { hp =>
          hp.participantId.toString == participant2.id.toString &&
          hp.permission == ParticipantPermission.Submission
        }
      }

    if (ok) {
      println(s"[bootstrap-multi] AUTHORIZED: $partyPrefix is hosted on participant2")
      return
    }

    println(s"[bootstrap-multi] Waiting for AUTHORIZED hosting of $partyPrefix on participant2 ...")
    Thread.sleep(1000)
    i += 1
  }
  sys.error(s"Timed out waiting for $partyPrefix to be AUTHORIZED-hosted on participant2")
}

// bench-owner: source=p1 + target=p2 co-sign
println("[bootstrap-multi] Multi-hosting bench-owner onto participant2 (co-sign required)...")
participant1.topology.party_to_participant_mappings.propose_delta(
  party = ownerPartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid1
)
participant2.topology.party_to_participant_mappings.propose_delta(
  party = ownerPartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid2
)
waitHostedOnP2("bench-owner")

// bench-counterparty2: source=p3 + target=p2 co-sign
println("[bootstrap-multi] Multi-hosting bench-counterparty2 onto participant2 (co-sign required)...")
participant3.topology.party_to_participant_mappings.propose_delta(
  party = counterparty2PartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid3
)
participant2.topology.party_to_participant_mappings.propose_delta(
  party = counterparty2PartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid2
)
waitHostedOnP2("bench-counterparty2")
