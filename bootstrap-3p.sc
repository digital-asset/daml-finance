// bootstrap-3p.sc (Canton OSS 3.4.10 compatible)
//
// 3 participants, RUNNER = participant1
// Goal: submit scripts via participant1 by multi-hosting required parties onto participant1 with Submission.
//
// Hosts:
//   participant1: (runner) alice, aliceBank     + Ledger API user lives here
//   participant2: bob, bobBank
//   participant3: centralBank
//

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
// Avoid sequencer NOT_SERVING race
println("[bootstrap] Waiting a bit for sequencer public API to become ready...")
Thread.sleep(8000)

// ----------------------------------------------------------------------------
// Connect participants (3)
participant1.synchronizers.connect_local(sequencer1, "mysynchronizer")
participant2.synchronizers.connect_local(sequencer1, "mysynchronizer")
participant3.synchronizers.connect_local(sequencer1, "mysynchronizer")

participant1.synchronizers.reconnect_local("mysynchronizer", true)
participant2.synchronizers.reconnect_local("mysynchronizer", true)
participant3.synchronizers.reconnect_local("mysynchronizer", true)

// Wait until active everywhere
def waitActive(alias: String, attempts: Int = 60): Unit = {
  var i = 0
  while (
    i < attempts && (
      !participant1.synchronizers.active(alias) ||
      !participant2.synchronizers.active(alias) ||
      !participant3.synchronizers.active(alias)
    )
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
// Enable parties (owned on specific participants)
val alicePartyId  = participant1.parties.enable("alice")
val aliceBankPartyId = participant1.parties.enable("aliceBank")

val bobPartyId = participant2.parties.enable("bob")
val bobBankPartyId = participant2.parties.enable("bobBank")

val centralBankPartyId = participant3.parties.enable("centralBank")

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

// ----------------------------------------------------------------------------
// Check vetting across all participants (using participant1 topology view)
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
// Multi-host helper: wait for EFFECTIVE hosting
def waitHostedEffective(
    where: com.digitalasset.canton.console.ParticipantReference,
    sid: com.digitalasset.canton.topology.SynchronizerId,
    partyId: PartyId,
    expectedHost: ParticipantId,
    attempts: Int = 60
): Unit = {
  var i = 0
  while (i < attempts) {
    val eff = where.topology.party_to_participant_mappings.list(
      synchronizerId = sid,
      proposals = false,
      timeQuery = TimeQuery.HeadState,
      operation = None,
      filterParty = partyId.toProtoPrimitive,
      filterParticipant = "",
      filterSigningKey = "",
      protocolVersion = None
    )

    val ok = eff.exists(r =>
      r.item.partyId.toProtoPrimitive == partyId.toProtoPrimitive &&
      r.item.participants.exists(hp =>
        hp.participantId.toString == expectedHost.toString &&
        hp.permission == ParticipantPermission.Submission
      )
    )

    if (ok) {
      println(s"[bootstrap] EFFECTIVE: ${partyId.toProtoPrimitive} hosted on ${expectedHost} with Submission (seen from ${where.name}).")
      return
    }

    println(s"[bootstrap] Waiting for EFFECTIVE hosting of ${partyId.toProtoPrimitive} on ${expectedHost} (seen from ${where.name}) ...")
    Thread.sleep(1000)
    i += 1
  }

  val props = where.topology.party_to_participant_mappings.list(
    synchronizerId = sid,
    proposals = true,
    timeQuery = TimeQuery.HeadState,
    operation = None,
    filterParty = partyId.toProtoPrimitive,
    filterParticipant = "",
    filterSigningKey = "",
    protocolVersion = None
  )
  println(s"[bootstrap] TIMEOUT. proposals=true entries (seen from ${where.name}) for ${partyId.toProtoPrimitive}: ${props.size}")
  props.foreach(println)

  val eff = where.topology.party_to_participant_mappings.list(
    synchronizerId = sid,
    proposals = false,
    timeQuery = TimeQuery.HeadState,
    operation = None,
    filterParty = partyId.toProtoPrimitive,
    filterParticipant = "",
    filterSigningKey = "",
    protocolVersion = None
  )
  println(s"[bootstrap] TIMEOUT. proposals=false entries (seen from ${where.name}) for ${partyId.toProtoPrimitive}: ${eff.size}")
  eff.foreach(println)

  sys.error(s"Timed out waiting for EFFECTIVE hosting of ${partyId.toProtoPrimitive} on ${expectedHost} (seen from ${where.name}).")
}

// ----------------------------------------------------------------------------
// Multi-host REQUIRED parties onto participant1 (RUNNER) with Submission
val pidRunner = participant1.id

// bob (owned by participant2) -> host on participant1
println("[bootstrap] Proposing: host bob on participant1 with Submission (proposal from participant2)...")
val txBob = participant2.topology.party_to_participant_mappings.propose_delta(
  party = bobPartyId,
  adds  = Seq(pidRunner -> ParticipantPermission.Submission),
  store = sid2,
  synchronize = Some(NonNegativeDuration.ofSeconds(60)),
  mustFullyAuthorize = false
)

println("[bootstrap] Authorizing bob proposal on participant1...")
participant1.topology.transactions.authorize(sid1, txBob.hash)

waitHostedEffective(participant2, sid2, bobPartyId, pidRunner)
waitHostedEffective(participant1, sid1, bobPartyId, pidRunner)


// bobBank (owned by participant2) -> host on participant1
println("[bootstrap] Proposing: host bobBank on participant1 with Submission (proposal from participant2)...")
val txBobBank = participant2.topology.party_to_participant_mappings.propose_delta(
  party = bobBankPartyId,
  adds  = Seq(pidRunner -> ParticipantPermission.Submission),
  store = sid2,
  synchronize = Some(NonNegativeDuration.ofSeconds(60)),
  mustFullyAuthorize = false
)

println("[bootstrap] Authorizing bobBank proposal on participant1...")
participant1.topology.transactions.authorize(sid1, txBobBank.hash)

waitHostedEffective(participant2, sid2, bobBankPartyId, pidRunner)
waitHostedEffective(participant1, sid1, bobBankPartyId, pidRunner)


// centralBank (owned by participant3) -> host on participant1 (keep if needed)
println("[bootstrap] Proposing: host centralBank on participant1 with Submission (proposal from participant3)...")
val txCentralBank = participant3.topology.party_to_participant_mappings.propose_delta(
  party = centralBankPartyId,
  adds  = Seq(pidRunner -> ParticipantPermission.Submission),
  store = sid3,
  synchronize = Some(NonNegativeDuration.ofSeconds(60)),
  mustFullyAuthorize = false
)

println("[bootstrap] Authorizing centralBank proposal on participant1...")
participant1.topology.transactions.authorize(sid1, txCentralBank.hash)

waitHostedEffective(participant3, sid3, centralBankPartyId, pidRunner)
waitHostedEffective(participant1, sid1, centralBankPartyId, pidRunner)

println("[bootstrap] bob + bobBank + centralBank are now hosted on participant1 (Submission).")

// ----------------------------------------------------------------------------
// Ledger API user on participant1 (runner)
val benchUserId = "bench-3p-runner-p1"
participant1.ledger_api.users.create(benchUserId)

participant1.ledger_api.users.rights.grant(
  id = benchUserId,
  actAs = Set(alicePartyId, aliceBankPartyId, bobPartyId, bobBankPartyId, centralBankPartyId),
  readAs = Set.empty,
  participantAdmin = false,
  identityProviderId = ""
)

println(s"[bootstrap] Created ledger user $benchUserId with actAs={alice,aliceBank,bob,bobBank,centralBank} on participant1")
println("[bootstrap] Done: 3p topology active, DAR vetted, required parties hosted on participant1 (Submission).")
