// bootstrap-3p-intermediated.sc (Canton OSS 3.4.10)
//
// Goal: ALL participants can submit as ALL parties
// Simple full-mesh Submission hosting
//
// Hosts:
//   participant1: alice
//   participant2: aliceBank, bob, bobBank
//   participant3: centralBank
//

// -----------------------------------------------------------------------------
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

println("[bootstrap] Waiting for sequencer...")
Thread.sleep(8000)

// -----------------------------------------------------------------------------
// Connect participants
Seq(participant1, participant2, participant3).foreach { p =>
  p.synchronizers.connect_local(sequencer1, "mysynchronizer")
  p.synchronizers.reconnect_local("mysynchronizer", true)
}

while (
  !participant1.synchronizers.active("mysynchronizer") ||
  !participant2.synchronizers.active("mysynchronizer") ||
  !participant3.synchronizers.active("mysynchronizer")
) {
  Thread.sleep(1000)
}

println("[bootstrap] All participants active")

val sid1 = participant1.synchronizers.id_of("mysynchronizer")
val sid2 = participant2.synchronizers.id_of("mysynchronizer")
val sid3 = participant3.synchronizers.id_of("mysynchronizer")

// -----------------------------------------------------------------------------
// Vetting helpers (MUST be defined before use)
def vettedPackageIds(pid: ParticipantId): Set[String] = {
  val v = participant1.topology.vetted_packages.list()
  v.find(_.item.participantId == pid).toSeq
    .flatMap(_.item.packages.map(_.packageId.toString))
    .toSet
}

def waitAllVetted(pkgIds: Seq[String]): Unit = {
  var ok = false
  while (!ok) {
    val p1 = vettedPackageIds(participant1.id)
    val p2 = vettedPackageIds(participant2.id)
    val p3 = vettedPackageIds(participant3.id)

    ok =
      pkgIds.forall(p1.contains) &&
      pkgIds.forall(p2.contains) &&
      pkgIds.forall(p3.contains)

    if (!ok) Thread.sleep(500)
  }
}

// -----------------------------------------------------------------------------
// Upload DAR FIRST (CRITICAL)
val dar =
  sys.env.getOrElse(
    "DAR",
    "/Users/karenstaner/daml-finance/package/test/daml/Daml.Finance.Benchmark.Test/.daml/dist/daml-finance-benchmark-test-0.99.0.20251211.0.dar"
  )
println(s"[bootstrap] Uploading DAR: $dar")

val before = participant1.packages.list().map(_.packageId).toSet

participant1.dars.upload(dar)
participant2.dars.upload(dar)
participant3.dars.upload(dar)

val after = participant1.packages.list().map(_.packageId).toSet
val newPkgIds = (after -- before).toSeq

println(s"[bootstrap] New packageIds: ${newPkgIds.mkString(", ")}")

waitAllVetted(newPkgIds)
println("[bootstrap] Packages vetted consistently on all participants")

// -----------------------------------------------------------------------------
// Enable parties (NOW SAFE)
val alice = participant1.parties.enable("alice")
val aliceBank= participant2.parties.enable("aliceBank")
val bob = participant2.parties.enable("bob")
val bobBank = participant2.parties.enable("bobBank")
val centralBank= participant3.parties.enable("centralBank")

println("[bootstrap] Parties enabled")

// -----------------------------------------------------------------------------
// Full mesh Submission hosting
def fullMeshSubmission(
    owner: com.digitalasset.canton.console.ParticipantReference,
    ownerSid: SynchronizerId,
    party: PartyId
): Unit = {

  val all = Seq(participant1.id, participant2.id, participant3.id)

  val tx = owner.topology.party_to_participant_mappings.propose_delta(
    party = party,
    adds = all.map(_ -> ParticipantPermission.Submission),
    store = ownerSid,
    mustFullyAuthorize = false
  )

  participant1.topology.transactions.authorize(sid1, tx.hash)
  participant2.topology.transactions.authorize(sid2, tx.hash)
  participant3.topology.transactions.authorize(sid3, tx.hash)
}

// -----------------------------------------------------------------------------
// Apply full mesh hosting
fullMeshSubmission(participant1, sid1, alice)
fullMeshSubmission(participant2, sid2, aliceBank)
fullMeshSubmission(participant2, sid2, bob)
fullMeshSubmission(participant2, sid2, bobBank)
fullMeshSubmission(participant3, sid3, centralBank)

println("[bootstrap] Full Submission mesh applied")

// -----------------------------------------------------------------------------
// Ledger API users
def createUser(p: com.digitalasset.canton.console.ParticipantReference, name: String): Unit = {
  p.ledger_api.users.create(name)
  p.ledger_api.users.rights.grant(
    id = name,
    actAs = Set(alice, aliceBank, bob, bobBank, centralBank),
    readAs = Set.empty,
    participantAdmin = false,
    identityProviderId = ""
  )
}

createUser(participant1, "bench-p1")
createUser(participant2, "bench-p2")
createUser(participant3, "bench-p3")

println("[bootstrap] DONE – topology + vetting are now correct")
