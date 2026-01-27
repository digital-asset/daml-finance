// bootstrap-2p.sc (2 participants)
// - create synchronizer
// - connect participants
// - enable parties (each on its own participant)
// - upload DAR + ensure packages vetted
// NO party replication here

// Bootstrap a synchronizer backed by sequencer1 + mediator1
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

// Connect both participants to the synchronizer
participant1.synchronizers.connect_local(sequencer1, "mysynchronizer")
participant2.synchronizers.connect_local(sequencer1, "mysynchronizer")

// Force reconnect (Canton 3.4.10 signature: (alias, force:Boolean))
participant1.synchronizers.reconnect_local("mysynchronizer", true)
participant2.synchronizers.reconnect_local("mysynchronizer", true)

// Wait until both participants are active on the synchronizer
def waitActive(alias: String, attempts: Int = 30): Unit = {
  var i = 0
  while (
    i < attempts &&
    (!participant1.synchronizers.active(alias)
      || !participant2.synchronizers.active(alias)
    )
  ) {
    println(s"[bootstrap] Waiting for participants to become active on $alias ...")
    Thread.sleep(1000)
    i += 1
  }
  require(participant1.synchronizers.active(alias), s"participant1 is still not active on $alias")
  require(participant2.synchronizers.active(alias), s"participant2 is still not active on $alias")
}

waitActive("mysynchronizer")
println("[bootstrap] All three participants are active on mysynchronizer.")

// Enable parties (after synchronizer is active)
val ownerPartyId = participant1.parties.enable("bench-owner")
val counterpartyPartyId = participant2.parties.enable("bench-counterparty")

println(s"[bootstrap] bench-owner partyId FULL:         ${ownerPartyId.toProtoPrimitive}")
println(s"[bootstrap] bench-counterparty partyId FULL:  ${counterpartyPartyId.toProtoPrimitive}")


// DAR path
val dar =
  sys.env.getOrElse(
    "DAR",
    "/Users/karenstaner/daml-finance/package/test/daml/Daml.Finance.Benchmark.Test/.daml/dist/daml-finance-benchmark-test-0.99.0.20251211.0.dar"
  )

println(s"[bootstrap] Using DAR: $dar")

// Capture package ids before upload (participant1 is enough for the diff)
val before = participant1.packages.list().map(_.packageId).toSet

// Upload DAR to both participants
val p1Main = participant1.dars.upload(dar)
val p2Main = participant2.dars.upload(dar)

println(s"[bootstrap] participant1 mainPackageId: $p1Main")
println(s"[bootstrap] participant2 mainPackageId: $p2Main")

// Compute which package IDs were added by this upload
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


val p1Missing = newPkgIds.filterNot(p1VettedPkgIdStrings.contains)
val p2Missing = newPkgIds.filterNot(p2VettedPkgIdStrings.contains)

println(s"[bootstrap] Missing vetted packages for participant1: ${p1Missing.mkString(", ")}")
println(s"[bootstrap] Missing vetted packages for participant2: ${p2Missing.mkString(", ")}")

require(p1Missing.isEmpty, "participant1 is missing vetted packages: " + p1Missing.mkString(", "))
require(p2Missing.isEmpty, "participant2 is missing vetted packages: " + p2Missing.mkString(", "))

println("[bootstrap] DAR uploaded and packages are vetted for both participants.")
