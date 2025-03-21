.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Account.V4
#######################

This package contains the *implementation* of accounts. It has the following module:

- :ref:`Account <module-daml-finance-account-v4-account-5834>`: Implementation of an account, i.e., a
  relationship between a custodian and an asset owner, referenced by holdings. It also provides an
  implementation of a factory from which you can create and remove accounts. Upon creation of an
  account, it allows you to specify controlling parties for incoming / outgoing transfers.

Changelog
*********
