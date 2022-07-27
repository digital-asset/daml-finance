# Daml.Finance.Interface.Settlement

This package contains type and interface definitions used to execute atomic settlement debits / credits / transfers of holdings.

An `Instruction` settles a single `Step` (the transfer of an asset from sender to receiver).

A `Settleable` is a generic interface for contracts that can be settled (typically a single `Instruction` or a batch thereof).

An `Instructable` is an interface for contracts that are used to generate `Instruction`s.
