.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Holding Upgrade
###############

This tutorial demonstrates how a custodian can upgrade their holding implementation for the
:ref:`Transferable <holding-standards>` holding standard to a custom, extended version in a *lazy*
manner. Specifically, the upgrade occurs seamlessly during a client's transfer action.

We are going to:

#. Introduce a new holding implementation, ``MyTransferable``, which extends the Daml Finance
   :ref:`Transferable <module-daml-finance-holding-v4-transferable-38649>` holding
   implementation. This new implementation differs in that it creates a
   ``MyTransferableTransferEvent`` contract instance with each transfer.
#. Replace the existing :ref:`HoldingFactory <module-daml-finance-holding-v4-factory-50391>`
   instance of the custodian with a new, compatible ``MyHoldingFactory`` for the ``MyTransferable``
   holding implementation.
#. Demonstrate that an upgrade happens upon a transfer of an old holding.

Run the Script
**************

You can execute this tutorial using the ``runUpgradeHolding`` script found in the
``UpgradeHolding.daml`` module. We'll examine the script step-by-step to demonstrate how it works in
practice.

Setup
=====

The script begins with ``runSetupAccountsAndHoldings``, initializing parties (a custodian Bank and
its clients, Alice and Bob), an account factory for the Bank, accounts for Alice and Bob, a holding
factory for the Bank, and a holding for Alice:

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeHolding.daml
  :language: daml
  :start-after: -- UPGRADE_HOLDING_SETUP_BEGIN
  :end-before: -- UPGRADE_HOLDING_SETUP_END

Initially, both the holding and holding factory are in the old version. The new ``MyTransferable``
and ``MyHoldingFactory`` implementations are available in the ``MyHolding.daml`` module.

Create a New Holding Factory
============================

To avoid creating holdings with the old implementation, we archive the Bank's holding factory:

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeHolding.daml
  :language: daml
  :start-after: -- UPGRADE_HOLDING_REMOVE_OLD_HOLDING_FACTORY_BEGIN
  :end-before: -- UPGRADE_HOLDING_REMOVE_OLD_HOLDING_FACTORY_END

Subsequently, we create a new holding factory with the same key for the Bank:

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeHolding.daml
  :language: daml
  :start-after: -- UPGRADE_HOLDING_CREATE_NEW_HOLDING_FACTORY_BEGIN
  :end-before: -- UPGRADE_HOLDING_CREATE_NEW_HOLDING_FACTORY_END

Note on Upgrade Rule
====================

Unlike the account upgrade tutorial, this process does not require an upgrade rule contract.

Client Upgrades
===============

We let Alice transfer her holding to Bob:

.. literalinclude:: ../../finance-upgrades/daml/Scripts/UpgradeHolding.daml
  :language: daml
  :start-after: -- UPGRADE_HOLDING_TRANSFER_BEGIN
  :end-before: -- UPGRADE_HOLDING_TRANSFER_END

As a result, Alice's existing holding is debited and Bob is credited a holding of the new version.

Summary
*******

Upon completing this tutorial, you should have a clear understanding of how to implement lazy
upgrades for holdings. The essential points to remember are:

* The old holding factory is to be replaced with a new one having the same key.
* Upgrades of holdings occur automatically during debiting and crediting actions, such as during
  transfers and settlement processes, facilitating a lazy upgrade approach.
