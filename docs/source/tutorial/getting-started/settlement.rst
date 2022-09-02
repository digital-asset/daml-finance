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
|                                      | An instruction is a contract where                                                                                                          |
|                                      | the sender can specify what holding they wish to send                                                                                       |
|                                      | and the receiver can specify on which account they wish to receive the holding.                                                             |
|                                      |                                                                                                                                             |
|                                      | The creation of Instructions is done using a                                                                                                |
|                                      | :ref:`Settlement Factory <type-daml-finance-interface-settlement-factory-factory-31525>` contract.                                          |
+--------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| 3. Allocate and approve instructions | For every instruction, the sender allocates a matching holding to it and the receiver specifies a receiving account.                        |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
+--------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+
| 4. Settle the batch                  | A :ref:`Batch <type-daml-finance-interface-settlement-batch-batch-97497>` contract is used  to settle all instructions atomically           |
|                                      | (transfering all allocated holdings to the corresponding receiving accounts).                                                               |
|                                      |                                                                                                                                             |
|                                      | This batch contract is created in step 2, together with the settlement instructions.                                                        |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
|                                      |                                                                                                                                             |
+--------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------+

Running the script
******************

This tutorial is executed in the ``Settlement`` script.

The first part is a repetition of the workflow of the previous tutorial. Only, in this case, two cash instruments are issued instead of just one.

The interesting bit starts once Alice proposes the FX trade to Bob. Before creating the trade proposal, we need to instantiate a :ref:`Settlement Factory <type-daml-finance-interface-settlement-factory-factory-31525>` contract.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- SETTLEMENT_FACTORY_BEGIN
  :end-before: -- SETTLEMENT_FACTORY_END

This is used to generate settlement instruction from settlement :ref:`steps <type-daml-finance-interface-settlement-types-step-78661>`.

Alice creates an ``FXTradeProposal`` template to propose the exchange of ``EUR`` against ``USD``.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- FX_PROPOSE_BEGIN
  :end-before: -- FX_PROPOSE_END

Bob then accepts the proposal, agreeing to the terms of the trade.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- FX_ACCEPT_BEGIN
  :end-before: -- FX_ACCEPT_END

Once the proposal is accepted, three contracts are created:

- an instruction to transfer ``1000 EUR`` from Alice to Bob
- an instruction to transfer ``1000 USD`` from Bob to Alice
- a batch contract to settle the two instructions atomically

The workflow to create these contracts makes use of the settlement factory.

.. literalinclude:: ../../../code-samples/getting-started/daml/Workflow/FXTrade.daml
  :language: daml
  :start-after: -- INSTRUCT_BEGIN
  :end-before: -- INSTRUCT_END

As a next step, Alice allocates her ``EUR`` holding to the corresponding instruction. Bob then approves the instruction specifying the receiving account.

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

Within the same transaction, Alice receives a ``USD`` holding from Bob in exchange for a ``EUR`` holding.

Further considerations
**********************

We now take a look at some aspects of the workflow and try to answer
some questions that you might be having.

If you are curious to see more use-cases for the library, feel free to
jump to the next tutorials and come back to this section when you feel
it is the right time.

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

Next steps
**********

You now know how to define complex transactions and settle them atomically.

In the next tutorial, we will introduce the lifecycling framework of the library, which is used to model the evolution of instruments.
The concepts of the settlement tutorial will be used to settle payments arising from lifecycling events.

