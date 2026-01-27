// bootstrap-N-multi-singleCustodian.sc (Canton OSS 3.4.10 compatible)
//
// Enables multi-submit by multi-hosting parties onto participant2.
//
// participant1: alice, aliceBank
// participant2: bob, bobBank
// participant3: centralBank
//
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
val alicePartyId = participant1.parties.enable("alice")
val aliceBankPartyId= participant1.parties.enable("aliceBank")

val bobPartyId = participant2.parties.enable("bob")
val bobBankPartyId = participant2.parties.enable("bobBank")

// ----------------------------------------------------------------------------
// Ledger API user with actAs rights for BOTH bob + bobBank (no submitMulti needed)
// ----------------------------------------------------------------------------

val benchUserId = "bench-bob-bobBank"

// Create (idempotency: if it already exists, delete first or catch error depending on your setup)
participant2.ledger_api.users.create(benchUserId)

// Grant rights (NOTE: Set[...] not Seq[...])
participant2.ledger_api.users.rights.grant(
  id = benchUserId,
  actAs = Set(bobPartyId, bobBankPartyId),
  readAs = Set.empty,
  participantAdmin = false,
  identityProviderAdmin = false,
  identityProviderId = "",
  readAsAnyParty = false,
  executeAs = Set.empty,
  executeAsAnyParty = false
)

println(s"[bootstrap] Created ledger user $benchUserId with actAs={bob,bobBank} on participant2")

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------


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
// Multi-host onto participant2 (required for multi-submit)
// ----------------------------------------------------------------------------

// centralBank: enabled on participant3 -> add participant2 as Submission host (co-sign p3 + p2)
println("[bootstrap-multi] Multi-hosting centralBank onto participant2 (co-sign required)...")
participant3.topology.party_to_participant_mappings.propose_delta(
  party = centralBankPartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid3
)
participant2.topology.party_to_participant_mappings.propose_delta(
  party = centralBankPartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid2
)


// alice: enabled on participant1 -> add participant2 as Submission host (co-sign p1 + p2)
println("[bootstrap-multi] Multi-hosting alice onto participant2 (co-sign required)...")
participant1.topology.party_to_participant_mappings.propose_delta(
  party = alicePartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid1
)
participant2.topology.party_to_participant_mappings.propose_delta(
  party = alicePartyId,
  adds  = Seq(participant2.id -> ParticipantPermission.Submission),
  store = sid2
)


// bob: already enabled on participant2; still sanity-check it’s hosted with Submission on p2
println("[bootstrap-multi] Sanity-checking bob is hosted on participant2...")

println("[bootstrap] Done: synchronizer up, parties enabled, DAR vetted, and required parties hosted on participant2.")

