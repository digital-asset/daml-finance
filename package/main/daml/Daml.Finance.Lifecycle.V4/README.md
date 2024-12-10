# Daml.Finance.Lifecycle

This package contains template implementations used to encode lifecycling logic that applies to
Instruments.

The `Effect` encodes the consequences of a lifecycle event for a unit of the target instrument. It
can be claimed using the `SettlementRule` contract.

The `ElectionEffect` is a single-use effect used for elections.
