# Daml.Finance.Interface.Settlement

This package contains type and interface definitions used to execute atomic settlement debits / credits / transfers of holdings.

An `Instruction` is an interface for providing instructions to settle a single `Step` (e.g., the direct transfer of an asset from a sender to a receiver account).

A `Batch` is an interface for settling multiple `Instruction`s in an atomic action.

A `Factory` is an interface for creating a `Batch` of `Instruction`s.
