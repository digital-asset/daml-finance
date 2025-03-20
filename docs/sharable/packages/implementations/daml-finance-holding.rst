.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Holding.V4
#######################

This package contains the *implementation* of holdings, including utility functions. It has the
following modules:

- :ref:`Fungible <module-daml-finance-holding-v4-fungible-60188>`: Implementation of a holding which is
  fungible only, i.e., include split and merge functionality
- :ref:`Transferable <module-daml-finance-holding-v4-transferable-38649>`: Implementation of a holding
  which is transferable only
- :ref:`TransferableFungible <module-daml-finance-holding-v4-transferablefungible-66907>`:
  Implementation of a holding which is both transferable and fungible
- :ref:`BaseHolding <module-daml-finance-holding-v4-baseholding-28133>`: Implementation of
  a holding which is neither transferable nor fungible
- :ref:`Util <module-daml-finance-holding-v4-util-71966>`: Utility functions related to holdings, e.g.,
  to transfer or split/merge a holding

The :doc:`Asset Model <../../concepts/asset-model>` page explains the relationship between
instruments, holdings, and accounts. Also, check out the
:doc:`Transfer tutorial <../../tutorials/getting-started/transfer>` for a description of how to
create a holding on an instrument and transfer it between accounts.

Changelog
*********
