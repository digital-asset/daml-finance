.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Account
####################

This package contains the *implementation* of accounts. It has the following module:

- :ref:`Account <module-daml-finance-account-account-19369>`: Implementation of an account,
  i.e., a relationship between a custodian and an asset owner, referenced by holdings. It also
  provides an implementation of a factory from which you can create and remove accounts. Upon
  creation of an account, it allows you to specify controlling parties for incoming / outgoing
  transfers.

Changelog
*********

.. toctree::
   :titlesonly:
   :maxdepth: 1

   Changelog <changelogs/daml-finance-account>
