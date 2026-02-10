// bootstrap-5p-intermediated.sc (Canton OSS 3.4.10)
//
// 5 participants, RUNNER = participant2
//
// Goal: ALL participants can submit as ALL parties (full-mesh Submission hosting)
// + all 5 parties live on different participants.
//
// Hosts:
//   participant1: alice
//   participant2: (runner) bob        + Ledger API user lives here
//   participant3: aliceBank
//   participant4: bobBank
//   participant5: centralBank


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
val ps = Seq(participant1, participant2, participant3, participant4, participant5)

ps.foreach { p =>
  p.synchronizers.connect_local(sequencer1, "mysynchronizer")
  p.synchronizers.reconnect_local("mysynchronizer", true)
}

while (ps.exists(p => !p.synchronizers.active("mysynchronizer"))) {
  Thread.sleep(1000)
}

println("[bootstrap] All participants active")

val sid1 = participant1.synchronizers.id_of("mysynchronizer")
val sid2 = participant2.synchronizers.id_of("mysynchronizer")
val sid3 = participant3.synchronizers.id_of("mysynchronizer")
val sid4 = participant4.synchronizers.id_of("mysynchronizer")
val sid5 = participant5.synchronizers.id_of("mysynchronizer")

// -----------------------------------------------------------------------------
// Vetting helpers 
def vettedPackageIds(pid: ParticipantId): Set[String] = {
  val v = participant1.topology.vetted_packages.list()
  v.find(_.item.participantId == pid).toSeq
    .flatMap(_.item.packages.map(_.packageId.toString))
    .toSet
}

def waitAllVetted(pkgIds: Seq[String]): Unit = {
  var ok = false
  while (!ok) {
    val v1 = vettedPackageIds(participant1.id)
    val v2 = vettedPackageIds(participant2.id)
    val v3 = vettedPackageIds(participant3.id)
    val v4 = vettedPackageIds(participant4.id)
    val v5 = vettedPackageIds(participant5.id)

    ok =
      pkgIds.forall(v1.contains) &&
      pkgIds.forall(v2.contains) &&
      pkgIds.forall(v3.contains) &&
      pkgIds.forall(v4.contains) &&
      pkgIds.forall(v5.contains)

    if (!ok) Thread.sleep(500)
  }
}

// -----------------------------------------------------------------------------
// Upload DAR  
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
participant4.dars.upload(dar)
participant5.dars.upload(dar)

val after = participant1.packages.list().map(_.packageId).toSet
val newPkgIds = (after -- before).toSeq

println(s"[bootstrap] New packageIds: ${newPkgIds.mkString(", ")}")

waitAllVetted(newPkgIds)
println("[bootstrap] Packages vetted consistently on all participants")

// -----------------------------------------------------------------------------
// Enable parties  — all on different participants
val alice = participant1.parties.enable("alice")
val bob = participant2.parties.enable("bob")         // runner-local party
val aliceBank = participant3.parties.enable("aliceBank")
val bobBank = participant4.parties.enable("bobBank")
val centralBank = participant5.parties.enable("centralBank")

println("[bootstrap] Parties enabled")

println(s"[bootstrap] alice FULL:        ${alice.toProtoPrimitive}")
println(s"[bootstrap] bob FULL:          ${bob.toProtoPrimitive}")
println(s"[bootstrap] aliceBank FULL:    ${aliceBank.toProtoPrimitive}")
println(s"[bootstrap] bobBank FULL:      ${bobBank.toProtoPrimitive}")
println(s"[bootstrap] centralBank FULL:  ${centralBank.toProtoPrimitive}")

// -----------------------------------------------------------------------------
// Full mesh Submission hosting
def fullMeshSubmission(
    owner: com.digitalasset.canton.console.ParticipantReference,
    ownerSid: SynchronizerId,
    party: PartyId
): Unit = {
  val all = Seq(participant1.id, participant2.id, participant3.id, participant4.id, participant5.id)

  val tx = owner.topology.party_to_participant_mappings.propose_delta(
    party = party,
    adds = all.map(_ -> ParticipantPermission.Submission),
    store = ownerSid,
    mustFullyAuthorize = false
  )
  participant1.topology.transactions.authorize(sid1, tx.hash)
  participant2.topology.transactions.authorize(sid2, tx.hash)
  participant3.topology.transactions.authorize(sid3, tx.hash)
  participant4.topology.transactions.authorize(sid4, tx.hash)
  participant5.topology.transactions.authorize(sid5, tx.hash)
}

// -----------------------------------------------------------------------------
// Apply full mesh hosting (for each party, propose from its owning participant)

fullMeshSubmission(participant1, sid1, alice)
fullMeshSubmission(participant2, sid2, bob)
fullMeshSubmission(participant3, sid3, aliceBank)
fullMeshSubmission(participant4, sid4, bobBank)
fullMeshSubmission(participant5, sid5, centralBank)

println("[bootstrap] Full Submission mesh applied")

// -----------------------------------------------------------------------------
// Ledger API users
// - Runner is participant2; create a bench user there with actAs = all parties.
// - also create optional users on other participants (handy for debugging).
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

createUser(participant2, "bench-runner-p2") // main runner user

// optional debug users
createUser(participant1, "bench-p1")
createUser(participant3, "bench-p3")
createUser(participant4, "bench-p4")
createUser(participant5, "bench-p5")

println("[bootstrap] DONE – 5p topology + vetting + full-mesh Submission are now correct (runner = participant2)")
