.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Holding
##############################

This package contains the *interface* and utility functions for holdings. It has the following
modules:

- :ref:`Factory <module-daml-finance-interface-holding-factory-6211>`:
  Interface for a holding factory used to create (credit) and archive (debit) holdings
- :ref:`Base <module-daml-finance-interface-holding-base-24195>`:
  Interface for a base holding which includes locking capabilities
- :ref:`Fungible <module-daml-finance-interface-holding-fungible-63712>`:
  Interface for a fungible holding which allows splitting and merging
- :ref:`Transferable <module-daml-finance-interface-holding-transferable-88121>`:
  Interface for a transferable holding, i.e., where ownership can be transferred to other
  parties
- :ref:`Util <module-daml-finance-interface-holding-util-81618>`:
  Utility functions related to holdings, e.g., getting the amount or the instrument of a holding

The :doc:`Asset Model <../../concepts/asset-model>` page explains the relationship between
instruments, holdings, and accounts. Check out the
:doc:`Transfer tutorial <../../tutorials/getting-started/transfer>` for a description on how to
create a holding on an instrument and how to transfer it between accounts.

The following diagram shows the incoming and outgoing dependencies for this package:

.. image:: ../../images/daml_finance_interface_holding.png
   :alt: A diagram showing the incoming and outgoing dependencies of the package.