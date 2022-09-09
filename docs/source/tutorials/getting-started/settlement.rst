.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Settlement
##########

This tutorial introduces the settlement features of the library through a simple example.
The purpose is to demonstrate how multiple holding transfers can be executed atomically.

We are going to

#. create a new ``TOKEN`` instrument
#. credit a token holding to Alice’s account
#. setup a delivery-vs-payment (DvP) transaction to exchange the two holdings
#. settle this transaction atomically

This example builds on the previous :doc:`Transfer <transfer>` tutorial script such that the same
accounts and existing holdings can be used.

Overview of the process
***********************

We first give a quick outline of the settlement process.

+--------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| 1. Define steps to be settled        | Two (or more) parties need to first agree on a set of :ref:`steps <type-daml-finance-interface-settlement-types-step-78661>` to be settled. |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
+--------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| 2. Generate settlement instructions  | :ref:`Instructions <type-daml-finance-interface-settlement-instruction-instruction-30569>` are generated for each step.                     |
|                                      | An instruction is a contract where the sender can specify its                                                                               |
|                                      | :ref:`Allocation <type-daml-finance-interface-settlement-types-allocation-46483>` preference for the instruction,                           |
|                                      | e.g., the matching holding they wish to send,                                                                                               |
|                                      | and the receiver can specify its                                                                                                            |
|                                      | :ref:`Approval <type-daml-finance-interface-settlement-types-approval-84286>` preference for the instruction,                               |
|                                      | e.g., the account they wish to receive the holding to.                                                                                      |
|                                      |                                                                                                                                             |
|                                      | The creation of Instructions is done using a                                                                                                |
|                                      | :ref:`Settlement Factory <type-daml-finance-interface-settlement-factory-factory-31525>` contract.                                          |
+--------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| 3. Allocate and approve instructions | For every instruction, the sender and receiver specify their allocation and approval preferences, respectively.                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
+--------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| 4. Settle the batch                  | A :ref:`Batch <type-daml-finance-interface-settlement-batch-batch-97497>` contract is used to settle all instructions atomically according  |
|                                      | to the specified preferences (e.g., by transfering all allocated holdings to the corresponding receiving accounts).                         |
|                                      |                                                                                                                                             |
|                                      | This batch contract is created in step 2, together with the settlement instructions.                                                        |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
+--------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+

Running the script
******************

The code for this tutorial can be executed via the ``runSettlement`` function in the ``Settlement.daml`` module.

The first part executes the script from the previous :doc:`Transfer <transfer>` tutorial to arrive at the initial state for this scenario.
We then create an additional *token* instrument and credit Alice's account with it.

The interesting bit starts once Alice proposes the DvP trade to Bob. Before creating the DvP proposal, we need to instantiate a :ref:`Settlement Factory <type-daml-finance-interface-settlement-factory-factory-31525>` contract.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- SETTLEMENT_FACTORY_BEGIN
  :end-before: -- SETTLEMENT_FACTORY_END

This is used to generate settlement instruction from settlement :ref:`steps <type-daml-finance-interface-settlement-types-step-78661>`.

Alice creates a ``Dvp.Proposal`` template to propose the exchange of the ``TOKEN`` against ``USD``.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- DVP_PROPOSE_BEGIN
  :end-before: -- DVP_PROPOSE_END

Bob then accepts the proposal, agreeing to the terms of the trade.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- DVP_ACCEPT_BEGIN
  :end-before: -- DVP_ACCEPT_END

Once the proposal is accepted, three contracts are created:

- an instruction to transfer ``10 TOKEN`` from Alice to Bob
- an instruction to transfer ``1000 USD`` from Bob to Alice
- a batch contract to settle the two instructions atomically

The workflow to create these contracts makes use of the settlement factory.

.. literalinclude:: ../../../code-samples/getting-started/daml/Workflow/DvP.daml
  :language: daml
  :start-after: -- INSTRUCT_BEGIN
  :end-before: -- INSTRUCT_END

As a next step, Alice allocates her ``TOKEN`` holding to the corresponding instruction. Bob then approves the instruction specifying the receiving account.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- ALLOCATE_APPROVE_BEGIN
  :end-before: -- ALLOCATE_APPROVE_END

The same happens in the second instruction (where Bob allocates his ``USD`` holding and Alice provides the receiving account).

Now that all instructions are fully allocated and approved, they can be finally settled.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- SETTLE_BEGIN
  :end-before: -- SETTLE_END

Within the same transaction, Alice receives a ``USD`` holding from Bob in exchange for a ``TOKEN`` holding.

Frequently Asked Questions
**************************

Why do we need a settlement factory?
====================================

A settlement factory contract is used to generate settlement ``Instructions`` from ``steps``.
It also generates a ``Batch`` contract which is used to settle instructions atomically.

The first reason why the factory is needed has already been introduced in the previous tutorial:
it provides an interface abstraction, so that your workflow does not need to depend on concrete implementations
of ``Batch`` or ``Instruction``.

A second aspect has to do with intermediated settlement.

Consider a real-world example where Alice instructs a bank transfer to send 100 USD to Bob. The following happens:

- 100 USD are debited from Alice's account at her bank
- 100 USD are transferred from Alice's bank to Bob's bank (via their accounts at the central bank)
- 100 USD are credited to Bob's account at his bank

A single ``Step`` requires three instructions to settle.

The same dynamics can be reproduced in Daml with the Settlement Factory, allowing for on-ledger intermediated settlement.
An example will be covered in one of the following tutorials.

Can we use a different settler?
===============================

In our example, Alice triggers the final settlement of the transaction (by exercising the ``Settle`` choice on the ``Batch`` contract).

In principle, a different settler could be chosen. The choice of a settler is usually quite delicate, as this party acquires visibility on the entire transaction and hence needs to be trusted.

Summary
*******

You now know how to define complex transactions and settle them atomically. The main points to take away are:

* A settlement factory is used to instruct settlement for an arbitrary list of steps
* Instructions are used to collect authorizations, assets to be moved, and means of settlement
* Batches group together instructions to be settled atomically

In the next tutorial, we will introduce the lifecycling framework of the library, which is used to model the evolution of instruments.
The concepts introduced in this tutorial will be used to settle payments arising from lifecycle events.

