# Daml.Finance.Interface.Lifecycle

This package contains interface definitions used to encode lifecycling logic that applies to
Instruments.

A `Lifecyclable` instrument can evolve according to certain trigger `Event`s. Once the action is
processed, it creates an `Effect`.

The `Effect` can be claimed by instrument holders in order to

- update their holdings to the new instrument version
- generate Settlement Instructions to settle the effect's consequences

A `Clock` contract is used to inject time information.
