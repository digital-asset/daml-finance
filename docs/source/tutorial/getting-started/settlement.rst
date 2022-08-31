.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Settlement
##########

This tutorial introduces the settlement features of the library through a simple example.
The purpose is to demonstrate how multiple holding transfers can be executed atomically.

We are going to

#. credit a ``EUR`` holding to Alice’s account
#. credit a ``USD`` holding to Bob’s account
#. setup an FX transaction to exchange the two holdings
#. settle this transaction atomically

This examples builds on the previous tutorial showcasing a simple transfer from Alice to Bob.

Overview of the Settlement process
**********************************

We give first a quick outline of the settlement process.

1. Define steps to be settled
=============================

Two (or more) parties need to first agree on a set of :ref:`steps <type-daml-finance-interface-settlement-types-step-78661>` to be settled.

2. Generate settlement instructions
===================================

:ref:`Instructions <type-daml-finance-interface-settlement-instruction-instruction-30569>` are generated for each step.
An instruction is a contract where
- the sender can specify what holding they wish to send
- the receiver can specify on which account they wish to receive the holding

The creation of Instructions is done using a :ref:`Settlement Factory <type-daml-finance-interface-settlement-factory-factory-31525>` contract.

3. Allocate and approve instructions
====================================

For every instruction
- the sender allocates a matching holding to it
- the receiver specifies a receiving account

4. Settle the batch
===================

A :ref:`Batch <type-daml-finance-interface-settlement-batch-batch-97497>` contract is used to settle all instructions atomically (transfering all allocated holdings to the corresponding receiving accounts).
This batch contract is created in step 2, together with the settlement instructions.

.. The ``Workflow`` folder includes an ``FXTradeProposal`` template, which is used to propose an exchange of two assets. These are specified as :ref:`instrument quantities <type-daml-finance-interface-instrument-base-instrument-q-62956>`.

.. Once the proposal is accepted by the counterparty, we generate
.. - settlement instructions for each step to be settled
.. - a batch contract used to settle all instructions atomically

.. Outline of the ``FXTradeProposal`` template
.. *******************************************

.. The ``Workflow`` folder includes an ``FXTradeProposal`` template, which is used to propose an exchange of two assets. These are specified as :ref:`instrument quantities <type-daml-finance-interface-instrument-base-instrument-q-62956>`.

.. Once the proposal is accepted by the counterparty, we generate
.. - settlement instructions for each step to be settled
.. - a batch contract used to settle all instructions atomically

Running the setup script
************************

Creating a ``Settlement factory``
=================================

The first part of the setup script is a repetition of the workflow of the previous tutorial.

The interesting bit starts once Alice proposes the FX trade to Bob.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- FX_PROPOSE_BEGIN
  :end-before: -- FX_PROPOSE_END

In order to create this contract, we need to pass in a :ref:`Settlement Factory <type-daml-finance-interface-settlement-factory-factory-31525>` contract.
This is used to generate settlement instructions given a set of :ref:`steps <type-daml-finance-interface-settlement-types-step-78661>`.

The ``Settlement Factory`` contract is created before the proposal and signed by the bank.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- BATCH_FACTORY_BEGIN
  :end-before: -- BATCH_FACTORY_END

Bob then accepts the proposal.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- FX_ACCEPT_BEGIN
  :end-before: -- FX_ACCEPT_END

Once the proposal is accepted, three additional contracts are created:
- an instruction to transfer ``1000 USD`` from Alice to Bob
- an instruction to transfer ``1000 EUR`` from Bob to Alice
- a batch contract to settle the two instructions atomically
.. - Atomic Dvp (Alice -> Bob and Bob -> Alice)

..   #. Instructing
..   #. Allocation
..   #. Approval
..   #. Settlement

.. Where can I explain the above pattern? Instruct, allocate, approve, settle

Further considerations
**********************

Why do we need a batch factory?
===============================

