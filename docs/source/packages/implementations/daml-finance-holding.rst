.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Holding
####################

This package contains the *implementation* of holdings, including utility functions. It has the
following modules:

- :ref:`Fungible <module-daml-finance-holding-fungible-7201>`: Implementation of a fungible
  holding, including split and merge functionality
- :ref:`NonFungible <module-daml-finance-holding-nonfungible-86571>`: Implementation of a
  non-fungible holding, which cannot be split or merged
- :ref:`NonTransferable <module-daml-finance-holding-nontransferable-44402>`: Implementation of
  a non-transferable holding
- :ref:`Util <module-daml-finance-holding-util-87323>`: Utility functions related to holdings,
  e.g., to transfer or lock/release a holding

The :doc:`Asset Model <../../concepts/asset-model>` page explains the relationship between
instruments, holdings, and accounts. Also, check out the
:doc:`Transfer tutorial <../../tutorials/getting-started/transfer>` for a description of how to
create a holding on an instrument and transfer it between accounts.

Changelog
*********

.. toctree::
   :titlesonly:
   :maxdepth: 1

   Changelog <changelogs/daml-finance-holding>

