.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Account Upgrade
###############

This tutorial presents a scenario where a custodian offers its clients the option to *voluntarily*
upgrade their "old" :ref:`Account <module-daml-finance-account-v4-account-5834>` contracts to a
"new" version. The focus is on demonstrating the process of such a voluntary upgrade.

We are going to:

#. Introduce ``MyAccount``, a custom account implementation for the custodian. It differs from
   the standard Daml Finance :ref:`Account <module-daml-finance-account-v4-account-5834>`
   implementation in that it does not implement the
   :ref:`Lockable <module-daml-finance-interface-util-v3-lockable-20339>`
   interface, making it a non-freezable account.
#. Prevent the custodian from creating old
   :ref:`Account <module-daml-finance-account-v4-account-5834>` instances, by archiving its
   existing
   :ref:`AccountFactory <module-daml-finance-account-v4-account-5834>`.
#. Instantiate a new account factory, ``MyAccountFactory``, which can create ``MyAccount``
   instances.
#. Provide a ``MyAccountUpgradeRule`` instance for clients, permitting them to upgrade their
   existing :ref:`Account <module-daml-finance-account-v4-account-5834>` instances to new
   ``MyAccount`` instances.
#. Upgrade the accounts for the clients.

Run the Script
**************

You can run this tutorial using the ``runUpgradeAccount`` script found in the
``UpgradeAccount.daml`` module. Let us examine it step-by-step.

Setup
=====

The script starts off with ``runSetupAccountsAndHoldings`` which sets up the necessary parties (a
custodian Bank and its clients, Alice and Bob), an account factory (used for creating accounts) for
the Bank, and accounts for both Alice and Bob. Additionally, it creates a holding for Alice. Since
the holding refers to its account by key, which remains unchanged, it does not require an upgrade
when the account is upgraded:

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeAccount.daml
  :language: daml
  :start-after: -- UPGRADE_ACCOUNT_SETUP_BEGIN
  :end-before: -- UPGRADE_ACCOUNT_SETUP_END

Initially, the accounts and the account factory are of the old version. The new implementations,
``MyAccount`` and ``MyAccountFactory``, can be found in the ``MyAccount.daml`` module.

Create a New Account Factory
============================

To prevent the creation of accounts of the old implementation, the first step involves archiving the
Bank's existing account factory:

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeAccount.daml
  :language: daml
  :start-after: -- UPGRADE_ACCOUNT_ARCHIVE_OLD_ACCOUNT_FACTORY_BEGIN
  :end-before: -- UPGRADE_ACCOUNT_ARCHIVE_OLD_ACCOUNT_FACTORY_END

Next, we instantiate a ``MyAccountFactory`` responsible for creating ``MyAccount`` instances:

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeAccount.daml
  :language: daml
  :start-after: -- UPGRADE_ACCOUNT_CREATE_NEW_ACCOUNT_FACTORY_BEGIN
  :end-before: -- UPGRADE_ACCOUNT_CREATE_NEW_ACCOUNT_FACTORY_END

Provide an Upgrade Rule
=======================

To offer the clients to upgrade their existing account to a ``MyAccount`` instance, we instantiate
a ``MyAccountUpgradeRule`` contract (which is also part of the ``MyAccount.daml`` module):

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeAccount.daml
  :language: daml
  :start-after: -- UPGRADE_ACCOUNT_CREATE_UPGRADE_RULE_BEGIN
  :end-before: -- UPGRADE_ACCOUNT_CREATE_UPGRADE_RULE_END

Clients Upgrade
===============

Following this, we let both Alice and Bob upgrade to the new account version:

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeAccount.daml
  :language: daml
  :start-after: -- UPGRADE_ACCOUNT_CLIENTS_UPGRADE_BEGIN
  :end-before: -- UPGRADE_ACCOUNT_CLIENTS_UPGRADE_END

Summary
*******

Upon completing this tutorial, you should have a solid understanding of how to manage voluntary
upgrades. The essential steps include:

* Replacing the old factory with a new, updated version.
* Introduce a rule contract that enables users to transition from the old to the new version,
  provided they are willing to do so.

In our next tutorial, we'll explore the process of upgrading holdings in a lazy manner.
