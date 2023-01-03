# Daml.Finance.Settlement

This package contains template implementations used to execute atomic settlement debits / credits /
transfers of holdings.

An `Instruction` settles a single `Step` (the transfer of an asset from sender to receiver). The
sender needs to allocate a holding. The receiver needs to provide an account to receive the holding.

A `Batch` is used atomically settled a batch of `Instruction`s.

The `BatchFactory` is used to create `Instruction`s and a corresponding `Batch` contract.

The `BatchFactoryWithIntermediaries` is similar but admits intermediated settlement of holdings
throughout an account hierarchy.

The `Transfer` template is used to settle a single-step transfer.
