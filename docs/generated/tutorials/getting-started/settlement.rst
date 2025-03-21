.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Settlement
##########

This tutorial introduces the settlement features of the library through a simple example. The
purpose is to demonstrate how multiple holding transfers can be executed atomically.

We are going to:

#. create a new ``TOKEN``
   :ref:`instrument <module-daml-finance-interface-instrument-token-v4-instrument-40238>`
#. credit a ``TOKEN`` holding to Alice’s account
#. setup a delivery-vs-payment (DvP) transaction to give Alice's ``TOKEN`` holding to Bob in
   exchange for a ``USD`` holding
#. settle this transaction atomically

This example builds on the previous :doc:`Transfer <transfer>` tutorial script in the sense that
the same accounts and the existing holdings are used.

Overview of the Process
***********************

We first give a quick outline of the settlement process:

+------------------+-------------------------------------------------------------------------------+
| 1. Define steps  | Two (or more) parties need to first agree on a set of                         |
|    to be settled | :ref:`steps <type-daml-finance-interface-settlement-v4-types-step-16302>`     |
|                  | to be settled.                                                                |
|                  |                                                                               |
+------------------+-------------------------------------------------------------------------------+
| 2. Generate      | :ref:`Instructions                                                            |
|    settlement    | <module-daml-finance-interface-settlement-v4-instruction-71097>`              |
|    instructions  | are generated for each step.                                                  |
|                  |                                                                               |
|                  | An instruction is a contract where the sender can specify its                 |
|                  | :ref:`Allocation                                                              |
|                  | <type-daml-finance-interface-settlement-v4-types-allocation-41200>`           |
|                  | preference for the instruction (e.g., the matching holding they wish to       |
|                  | send). The receiver can specify its :ref:`Approval                            |
|                  | <type-daml-finance-interface-settlement-v4-types-approval-77821>`             |
|                  | preference for the instruction (e.g., the account they wish to receive        |
|                  | the holding to).                                                              |
|                  |                                                                               |
|                  | The creation of Instructions is done by first using a :ref:`Route Provider    |
|                  | <type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628>`|
|                  | and then applying a :ref:`Settlement Factory                                  |
|                  | <module-daml-finance-interface-settlement-v4-factory-85379>`.                 |
|                  |                                                                               |
+------------------+-------------------------------------------------------------------------------+
| 3. Allocate and  | For every instruction, the sender and the receiver specify their allocation   |
|    approve       | and approval preferences, respectively.                                       |
|    instructions  |                                                                               |
|                  |                                                                               |
+------------------+-------------------------------------------------------------------------------+
| 4. Settle the    | A :ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>`      |
|    batch         | contract is used to settle all instructions atomically according to the       |
|                  | specified preferences (e.g. by transferring all allocated holdings to the     |
|                  | corresponding receiving accounts).                                            |
|                  |                                                                               |
|                  | This batch contract is created in step 2, together with the settlement        |
|                  | instructions.                                                                 |
|                  |                                                                               |
+------------------+-------------------------------------------------------------------------------+

Run the Script
**************

The code for this tutorial can be executed via the ``runSettlement`` function in the
``Settlement.daml`` module.

The first part executes the script from the previous :doc:`Transfer <transfer>` tutorial to arrive
at the initial state for this scenario. We then create an additional ``TOKEN``
:ref:`instrument <module-daml-finance-interface-instrument-token-v4-instrument-40238>`
and credit Alice's account with it.

The interesting part begins once Bob proposes the DvP trade to Alice. Before creating the DvP
proposal, we need to instantiate two contracts:

1. :ref:`Route Provider <type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628>`

   .. literalinclude:: ../../quickstart-finance/daml/Scripts/Settlement.daml
     :language: daml
     :start-after: -- ROUTE_PROVIDER_BEGIN
     :end-before: -- ROUTE_PROVIDER_END

   This is used to discover a settlement route, i.e.,
   :ref:`routed steps <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`, for each
   settlement :ref:`step <type-daml-finance-interface-settlement-v4-types-step-16302>`. In this
   example, the route provider simply converts each step to a routed step using a single custodian
   (the bank).

2. :ref:`Settlement Factory <module-daml-finance-interface-settlement-v4-factory-85379>`

   .. literalinclude:: ../../quickstart-finance/daml/Scripts/Settlement.daml
     :language: daml
     :start-after: -- SETTLEMENT_FACTORY_BEGIN
     :end-before: -- SETTLEMENT_FACTORY_END

   This is used to generate the settlement batch and instructions from the
   :ref:`routed steps <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`.

Bob creates a ``Dvp.Proposal`` template to propose the exchange of the ``TOKEN`` against ``USD``.

.. literalinclude:: ../../quickstart-finance/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- DVP_PROPOSE_BEGIN
  :end-before: -- DVP_PROPOSE_END

Alice then accepts the proposal, agreeing to the terms of the trade.

.. literalinclude:: ../../quickstart-finance/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- DVP_ACCEPT_BEGIN
  :end-before: -- DVP_ACCEPT_END

Once the proposal is accepted, three contracts are created:

- an instruction to transfer ``10 TOKEN`` from Alice to Bob
- an instruction to transfer ``USD 1000`` from Bob to Alice
- a batch contract to settle the two instructions atomically

The workflow to create these contracts makes use of the route provider and the settlement factory.

.. literalinclude:: ../../quickstart-finance/daml/Workflow/DvP.daml
  :language: daml
  :start-after: -- INSTRUCT_BEGIN
  :end-before: -- INSTRUCT_END

As a next step, Alice allocates her ``TOKEN`` holding to the corresponding instruction. Bob then
approves the instruction specifying the receiving account.

.. literalinclude:: ../../quickstart-finance/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- ALLOCATE_APPROVE_BEGIN
  :end-before: -- ALLOCATE_APPROVE_END

The same happens in the second instruction (where Bob allocates his ``USD`` holding and Alice
provides the receiving account).

Now that all instructions are fully allocated and approved, they can finally be settled.

.. literalinclude:: ../../quickstart-finance/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- SETTLE_BEGIN
  :end-before: -- SETTLE_END

Within the same transaction, Alice receives a ``USD`` holding from Bob in exchange for a ``TOKEN``
holding.

Frequently Asked Questions
**************************

Why do we need a route provider?
====================================

Consider a real-world example where Bob instructs a bank transfer to send USD 100 to Alice. The
following happens:

- ``USD 100`` are debited from Bob's account at his bank
- ``USD 100`` are transferred from Bob's bank to Alice's bank (via their accounts at the central
  bank)
- ``USD 100`` are credited to Alice's account at her bank

A single settlement :ref:`Step <type-daml-finance-interface-settlement-v4-types-step-16302>` requires
three :ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`\s to settle.

The same dynamics can be reproduced in Daml with a :ref:`Route Provider
<type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628>` implementation, allowing
for on-ledger intermediated settlement. For example, see the
:doc:`Intermediated Lifecycling <../advanced-topics/lifecycling/intermediated-lifecycling>`
tutorial.

Why do we need a settlement factory?
====================================

A settlement factory contract is used to generate settlement
:ref:`Instructions <module-daml-finance-interface-settlement-v4-instruction-71097>` from
:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`\s.
It also generates a :ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>`
contract, which is used to settle instructions atomically.

The reason why the factory is needed has already been introduced in the previous tutorial: it
provides an interface abstraction, so that your workflow does not need to depend on concrete
implementations of :ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>`
or :ref:`Instructions <module-daml-finance-interface-settlement-v4-instruction-71097>`.

Can we use a different settler?
===============================

In our example, Bob triggers the final settlement of the transaction (by exercising the ``Settle``
choice on the :ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>` contract).

In principle, a different settler could be chosen. The choice of a settler is usually quite
delicate, as this party acquires visibility on the entire transaction and hence needs to be trusted.

What if one party wants to cancel the settlement?
=================================================

The parties who sign the
:ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>` contract (the instructor and
consenters) can exercise the ``Cancel`` choice of the
:ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>` to cancel all associated
:ref:`Instructions <module-daml-finance-interface-settlement-v4-instruction-71097>`
atomically.

Summary
*******

You know how to define complex transactions and settle them atomically. The main points to take away
are:

* A route provider is used to discover settlement routes, i.e., routed steps, for each settlement
  step.
* A settlement factory is used to instruct settlement for an arbitrary list of routed steps.
* Instructions are used to collect authorizations, assets to be moved, and means of settlement.
* Batches group together instructions to be settled atomically.

In the next tutorial, we will introduce the lifecycling framework of the library, which is used to
model the evolution of instruments. The concepts introduced in this tutorial will be used to settle
payments arising from lifecycle events.
