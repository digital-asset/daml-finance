.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Account
##############################

This package contains the *interface* and utility functions for accounts. It has the following
modules:

- :ref:`Factory <module-daml-finance-interface-account-factory-11691>`:
  Interface that allows implementing templates to create and remove accounts
- :ref:`Account <module-daml-finance-interface-account-account-92922>`:
  Interface which represents an established relationship between a custodian and an owner. It
  specifies parties controlling incoming and outgoing transfers, and allows for crediting and
  debiting holdings
- :ref:`Util <module-daml-finance-interface-account-util-56106>`:
  Utility functions related to accounts, e.g., getting the custodian or the owner of an account
