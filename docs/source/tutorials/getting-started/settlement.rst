.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Settlement
##########

This tutorial introduces the settlement features of the library through a simple example. The
purpose is to demonstrate how multiple holding transfers can be executed atomically.

We are going to:

#. create a new ``TOKEN``
   :ref:`instrument <type-daml-finance-interface-instrument-token-instrument-instrument-4350>`
#. credit a ``TOKEN`` holding to Alice’s account
#. setup a delivery-vs-payment (DvP) transaction to give Alice's ``TOKEN`` holding to Bob in exchange
   for a ``USD`` holding
#. settle this transaction atomically

This example builds on the previous :doc:`Transfer <transfer>` tutorial script in the sense that
the same accounts and the existing holdings are used.

Overview of the Process
***********************

We first give a quick outline of the settlement process:

+--------------------+-----------------------------------------------------------------------------+
| 1. Define steps to | Two (or more) parties need to first agree on a set of                       |
|    be settled      | :ref:`steps <type-daml-finance-interface-settlement-types-step-78661>`      |
|                    | to be settled.                                                              |
|                    |                                                                             |
+--------------------+-----------------------------------------------------------------------------+
| 2. Generate        | :ref:`Instructions                                                          |
|    settlement      | <type-daml-finance-interface-settlement-instruction-instruction-30569>`     |
|    instructions    | are generated for each step.                                                |
|                    |                                                                             |
|                    | An instruction is a contract where the sender can specify its               |
|                    | :ref:`Allocation                                                            |
|                    | <type-daml-finance-interface-settlement-types-allocation-46483>`            |
|                    | preference for the instruction (e.g., the matching holding they wish to     |
|                    | send). The receiver can specify its :ref:`Approval                          |
|                    | <type-daml-finance-interface-settlement-types-approval-84286>`              |
|                    | preference for the instruction (e.g., the account they wish to receive      |
|                    | the holding to).                                                            |
|                    |                                                                             |
|                    | The creation of Instructions is done by first using a :ref:`Route Provider  |
|                    | <type-daml-finance-interface-settlement-routeprovider-routeprovider-53805>` |
|                    | and then applying a :ref:`Settlement Factory                                |
|                    | <type-daml-finance-interface-settlement-factory-factory-31525>`.            |
|                    |                                                                             |
+--------------------+-----------------------------------------------------------------------------+
| 3. Allocate and    | For every instruction, the sender and the receiver specify their allocation |
|    approve         | and approval preferences, respectively.                                     |
|    instructions    |                                                                             |
|                    |                                                                             |
+--------------------+-----------------------------------------------------------------------------+
| 4. Settle the      | A :ref:`Batch <type-daml-finance-interface-settlement-batch-batch-97497>`   |
|    batch           | contract is used to settle all instructions atomically according to the     |
|                    | specified preferences (e.g. by transfering all allocated holdings to the    |
|                    | corresponding receiving accounts).                                          |
|                    |                                                                             |
|                    | This batch contract is created in step 2, together with the settlement      |
|                    | instructions.                                                               |
|                    |                                                                             |
+--------------------+-----------------------------------------------------------------------------+

Run the Script
**************

The code for this tutorial can be executed via the ``runSettlement`` function in the
``Settlement.daml`` module.

The first part executes the script from the previous :doc:`Transfer <transfer>` tutorial to arrive
at the initial state for this scenario. We then create an additional ``TOKEN``
:ref:`instrument <type-daml-finance-interface-instrument-token-instrument-instrument-4350>`
and credit Alice's account with it.

The interesting part begins once Alice proposes the DvP trade to Bob. Before creating the DvP
proposal, we need to instantiate two contracts:

1. :ref:`Route Provider <type-daml-finance-interface-settlement-routeprovider-routeprovider-53805>`

   .. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
     :language: daml
     :start-after: -- ROUTE_PROVIDER_BEGIN
     :end-before: -- ROUTE_PROVIDER_END

   This is used to discover a settlement route, i.e.,
   :ref:`routed steps <type-daml-finance-interface-settlement-types-routedstep-10086>`, for each
   settlement :ref:`step <type-daml-finance-interface-settlement-types-step-78661>`. In this
   example, the route provider simply converts each step to a routed step using a single custodian (the bank).

2. :ref:`Settlement Factory <type-daml-finance-interface-settlement-factory-factory-31525>`

   .. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
     :language: daml
     :start-after: -- SETTLEMENT_FACTORY_BEGIN
     :end-before: -- SETTLEMENT_FACTORY_END

   This is used to generate the settlement batch and instructions from the
   :ref:`routed steps <type-daml-finance-interface-settlement-types-routedstep-10086>`.

Bob creates a ``Dvp.Proposal`` template to propose the exchange of the ``TOKEN`` against ``USD``.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- DVP_PROPOSE_BEGIN
  :end-before: -- DVP_PROPOSE_END

Alice then accepts the proposal, agreeing to the terms of the trade.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- DVP_ACCEPT_BEGIN
  :end-before: -- DVP_ACCEPT_END

Once the proposal is accepted, three contracts are created:

- an instruction to transfer ``10 TOKEN`` from Alice to Bob
- an instruction to transfer ``USD 1000`` from Bob to Alice
- a batch contract to settle the two instructions atomically

The workflow to create these contracts makes use of the route provider and the settlement factory.

.. literalinclude:: ../../../code-samples/getting-started/daml/Workflow/DvP.daml
  :language: daml
  :start-after: -- INSTRUCT_BEGIN
  :end-before: -- INSTRUCT_END

As a next step, Alice allocates her ``TOKEN`` holding to the corresponding instruction. Bob then
approves the instruction specifying the receiving account.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- ALLOCATE_APPROVE_BEGIN
  :end-before: -- ALLOCATE_APPROVE_END

The same happens in the second instruction (where Bob allocates his ``USD`` holding and Alice
provides the receiving account).

Now that all instructions are fully allocated and approved, they can finally be settled.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Settlement.daml
  :language: daml
  :start-after: -- SETTLE_BEGIN
  :end-before: -- SETTLE_END

Within the same transaction, Alice receives a ``USD`` holding from Bob in exchange for a ``TOKEN``
holding.

Frequently Asked Questions
**************************

Why do we need a route provider?
====================================

Consider a real-world example where Alice instructs a bank transfer to send USD 100 to Bob. The
following happens:

- ``USD 100`` are debited from Alice's account at her bank
- ``USD 100`` are transferred from Alice's bank to Bob's bank (via their accounts at the central bank)
- ``USD 100`` are credited to Bob's account at his bank

A single settlement :ref:`Step <type-daml-finance-interface-settlement-types-step-78661>` requires
three :ref:`RoutedStep <type-daml-finance-interface-settlement-types-routedstep-10086>`\s to settle.

The same dynamics can be reproduced in Daml with a :ref:`Route Provider
<type-daml-finance-interface-settlement-routeprovider-routeprovider-53805>` implementation, allowing
for on-ledger intermediated settlement. For example, see the
:doc:`Intermediated Lifecycling <../instrument-modeling/intermediated-lifecycling>` tutorial.

Why do we need a settlement factory?
====================================

A settlement factory contract is used to generate settlement
:ref:`Instructions <type-daml-finance-interface-settlement-instruction-instruction-30569>` from
:ref:`RoutedStep <type-daml-finance-interface-settlement-types-routedstep-10086>`\s.
It also generates a :ref:`Batch <type-daml-finance-interface-settlement-batch-batch-97497>`
contract, which is used to settle instructions atomically.

The reason why the factory is needed has already been introduced in the previous tutorial: it
provides an interface abstraction, so that your workflow does not need to depend on concrete
implementations of :ref:`Batch <type-daml-finance-interface-settlement-batch-batch-97497>`
or :ref:`Instructions <type-daml-finance-interface-settlement-instruction-instruction-30569>`.

Can we use a different settler?
===============================

In our example, Alice triggers the final settlement of the transaction (by exercising the ``Settle``
choice on the :ref:`Batch <type-daml-finance-interface-settlement-batch-batch-97497>` contract).

In principle, a different settler could be chosen. The choice of a settler is usually quite
delicate, as this party acquires visibility on the entire transaction and hence needs to be trusted.

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
