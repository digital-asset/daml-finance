.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Account
####################

This package contains the *implementation* and utility functions for accounts. It has the
following modules:

- :ref:`Account <module-daml-finance-account-account-19369>`: Implementation of an account,
  i.e., a relationship between a custodian and an asset owner, referenced by holdings. It also
  provides an implementation of a factory from which you can create and remove accounts. Upon
  creation of an account, it allows you to specify controlling parties for incoming / outgoing
  transfers
- :ref:`Util <module-daml-finance-account-util-35751>`: Utility functions related to accounts,
  e.g., an atomic credit-and-debit of holdings
