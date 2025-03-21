.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Settlement
##########

:ref:`Settlement <settlement>` refers to the execution of holding transfers originating from a
financial transaction.

Daml Finance provides facilities to execute these transfers atomically (i.e., within the same Daml
transaction). Interfaces are defined in the ``Daml.Finance.Interface.Settlement.V4`` package, whereas
implementations are provided in the ``Daml.Finance.Settlement.V4`` package.

In this section, we first illustrate the settlement workflow with the help of an example FX
transaction, where Alice transfers a EUR-denominated holding to Bob, in exchange for a
USD-denominated holding of the same amount.

We then delve into the details of each of the settlement components.

Workflow
********

Our initial state looks as follows:

* Alice owns a holding on a ``EUR`` instrument, for an amount of ``1000``
* Bob owns a holding on a ``USD`` instrument, for an amount of ``1000``

.. image:: ../images/settlement_initial_state.png
   :alt: Alice owns a EUR holding, Bob owns a USD holding.

These holdings are generally held at different custodians.

Instruct
========

Alice and Bob want to exchange their holdings and agree to enter into the transaction by being
signatories on a transaction contract. Settlement can then be instructed which results in 3
contract instances being created:

#. an :ref:`Instruction <module-daml-finance-settlement-v4-instruction-73130>`
   to transfer EUR 1000 from Alice to Bob
#. an :ref:`Instruction <module-daml-finance-settlement-v4-instruction-73130>`
   to transfer USD 1000 from Bob to Alice
#. a :ref:`Batch <module-daml-finance-settlement-v4-batch-88124>`
   used to execute the above Instructions

.. image:: ../images/settlement_instructed.png
   :alt: Settlement is instructed.

Each instruction defines who is the sender, who is the receiver, and what should be transferred
(instrument and amount) at which custodian.

Allocate and Approve
====================

In order to execute the FX transaction, we first need to:

- allocate, i.e., specify which holding should be used
- approve, i.e., specify to which account the asset should be transferred

Allocation and approval is required for
each :ref:`Instruction <module-daml-finance-settlement-v4-instruction-73130>`.

Alice :ref:`allocates <module-daml-finance-interface-settlement-v4-instruction-71097>` the instruction
where she is the sender by pledging her holding. Bob does the same on the instruction where he is
the sender.

.. image:: ../images/settlement_allocated.png
   :alt: Settlement is allocated.

Each receiver can then specify to which account the holding should be sent by
:ref:`approving <module-daml-finance-interface-settlement-v4-instruction-71097>`
the corresponding instruction.

.. image:: ../images/settlement_allocated_approved.png
   :alt: Settlement is allocated and approved.

Execute
=======

Once both instructions are allocated and approved, a Settler party uses the
:ref:`Batch <module-daml-finance-settlement-v4-batch-88124>` contract to
:ref:`execute <module-daml-finance-interface-settlement-v4-instruction-71097>`
them and finalize settlement in one atomic transaction.

.. image:: ../images/settlement_executed.png
   :alt: Settlement is allocated and approved.

The instructions and the batch are archived following a successful execution.

Remarks
=======

There are some assumptions that need to hold in order for the settlement to work in practice:

- Bob needs to have an account at the custodian where Alice's holding is held and vice versa (for
  an example with intermediaries, see `Route provider`_ below.
- Both holdings need to be
  :ref:`Transferable <module-daml-finance-interface-holding-v4-transferable-93054>`
- The transfer must be fully authorized (i.e., the parties allocating and approving an instruction
  must be the controllers of outgoing and incoming transfers of the corresponding accounts,
  respectively)

Also, note that the allocation and approval steps can happen in any order.

The components in detail
************************

Route provider
==============

When a transfer requires intermediaries to be involved, the role of a
:ref:`Route Provider <type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628>`
becomes important. Let us assume, for instance, that Alice's EUR holding in the example above is
held at Bank A, whereas Bob has a EUR account at Bank B. Bank A and Bank B both have accounts at the
Central Bank.

.. image:: ../images/settlement_hierarchy.png
   :alt: Hierarchical account structure. Alice has an account at Bank A. Bob has an account at
         Bank B. Bank A and Bank B have an account at the Central Bank.

In this case, a direct holding transfer from Alice to Bob cannot generally be instructed. The
original :ref:`Instruction <module-daml-finance-settlement-v4-instruction-73130>`
between Alice and Bob needs to be replaced by three separate
:ref:`Instructions <module-daml-finance-settlement-v4-instruction-73130>`:

- **1A**: Alice sends EUR 1000 (held at Bank A) to Bank A
- **1B**: Bank A sends EUR 1000 (held at the Central Bank) to Bank B.
- **1C**: Bank B credits EUR 1000 to Bob's account (held at Bank B)

.. image:: ../images/settlement_hierarchy_instructed.png
   :alt: Instructions for intermediated settlement: Alice sends EUR 1000 to Bank A. Bank A sends
         EUR 1000 to Bank B. Bank B sends EUR 1000 to Bob.

We refer to this scenario as *settlement with intermediaries*, or just *intermediated settlement*.

The Route Provider is used to discover a settlement route, i.e.,
:ref:`routed steps <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`, for each
settlement :ref:`step <type-daml-finance-interface-settlement-v4-types-step-16302>`.

Settlement factory
==================

The :ref:`Settlement Factory <module-daml-finance-interface-settlement-v4-factory-85379>` is used
to instruct settlement, i.e., create the :ref:`Batch <module-daml-finance-settlement-v4-batch-88124>`
contract and the settlement :ref:`Instructions <module-daml-finance-settlement-v4-instruction-73130>`,
from :ref:`routed steps <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`, so that
they can be allocated and approved by the respective parties.

Instruction
===========

The :ref:`Instruction <module-daml-finance-interface-settlement-v4-instruction-71097>` is
used to settle a single holding transfer at a specific custodian, once it is ``allocated`` and
``approved``.

In the :ref:`Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200>` step, the
sender acknowledges the transfer and determines how to send the holding. This is usually done by
allocating with a :ref:`Pledge <constr-daml-finance-interface-settlement-v4-types-pledge-57866>`
of the sender's existing holding (which has the correct instrument quantity) at the custodian. When
the sender is also the custodian, the instruction can be allocated with
:ref:`CreditReceiver <constr-daml-finance-interface-settlement-v4-types-creditreceiver-33781>`. In this
case, a new holding is directly credited into the receiver's account.

In the :ref:`Approval <type-daml-finance-interface-settlement-v4-types-approval-77821>` step, the
receiver acknowledges the transfer and determines how to receive the holding. This is usually done
by approving with
:ref:`TakeDelivery <constr-daml-finance-interface-settlement-v4-types-takedelivery-31030>` to one of
the receiver's accounts at the custodian. When the receiver is also the incoming holding's
custodian, the instruction can be approved with
:ref:`DebitSender <constr-daml-finance-interface-settlement-v4-types-debitsender-18125>`. In this case,
the holding is directly debited from the sender's account. A holding owned by the custodian at the
custodian has no economical value, it is a liability against themselves and can therefore be
archived without consequence.

To clarify these concepts, here is how the 3 instructions in the intermediated example above would
be allocated / approved.

+----------------------------------------------------+----------------------+----------------------+
| Instruction                                        | Allocation           | Approval             |
+====================================================+======================+======================+
| 1A : EUR 1000 from Alice to Bank A @ Bank A        | Alice pledges her    | Bank A approves      |
|                                                    | holding              | with DebitSender     |
+----------------------------------------------------+----------------------+----------------------+
| 1B : EUR 1000 from Bank A to Bank B @ Central Bank | Bank A pledges       | Bank B takes delivery|
|                                                    | its holding          | to its account       |
+----------------------------------------------------+----------------------+----------------------+
| 1C : EUR 1000 from Bank B to Bob @ Bank B          | Bank B allocates     | Bob takes delivery   |
|                                                    | with CreditReceiver  | to his account       |
+----------------------------------------------------+----------------------+----------------------+

Finally, the :ref:`Instruction <module-daml-finance-settlement-v4-instruction-73130>` supports two
additional settlement modes:

- Any instruction can settle off-ledger (if the stakeholders agree to do so). For this to work, we
  require the custodian and the sender to jointly allocate the instruction with a
  :ref:`SettleOffledger <constr-daml-finance-interface-settlement-v4-types-settleoffledger-82795>`,
  and the custodian and the receiver to jointly approve the instruction with a
  :ref:`SettleOffledgerAcknowledge
  <constr-daml-finance-interface-settlement-v4-types-settleoffledgeracknowledge-65556>`.
- A special case occurs when a transfer happens via an intermediary at the same custodian, i.e., we
  have 2 instructions having the same custodian and instrument quantity (in a batch), and the
  receiver of the first instruction is the same as the sender of the second instruction. In this
  case, we allow the holding received from the first instruction to be passed through to settle the
  second instruction, i.e., without using any pre-existing holding of the intermediary. For this to
  work, the first instruction is approved with
  :ref:`PassThroughTo <constr-daml-finance-interface-settlement-v4-types-passthroughto-8399>` (i.e.,
  pass through to the second instruction), and the second instruction is allocated with
  :ref:`PassThroughFrom <constr-daml-finance-interface-settlement-v4-types-passthroughfrom-2474>`
  (i.e., pass through from the first instruction). An intermediary account used for the passthrough
  is thereby also to be specified.

Batch
=====

The :ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>` is used to execute a set
of instructions atomically. Execution will fail if any of the
:ref:`Instructions <module-daml-finance-settlement-v4-instruction-73130>` is not fully allocated
/ approved, or if the transfer is unsuccessful.

Settlement Time
===============

The settlement time for financial transactions can vary based on the market and the type of
securities involved. Typically, settlement periods are denoted as T+1, T+2, or T+3, indicating the
transaction date plus one, two, or three business days, respectively. Some markets may also offer
real-time settlement options. It's also common for certain trades between parties to have unique,
mutually agreed-upon settlement periods.

The :ref:`Batch <module-daml-finance-settlement-v4-batch-88124>` and
:ref:`Instruction <module-daml-finance-settlement-v4-instruction-73130>`
implementations are designed to allow the optional setting of a preferred settlement time, without
any mechanisms that enforce a specific settlement time. This design choice offers several
advantages:

- **Settlement Time Flexibility:** The Settler party has the discretion to decide or defer the
  actual settlement time. This includes the ability to settle transactions after the designated
  settlement time, which can be particularly useful in resolving any off-ledger disputes.

- **Avoidance of Early Settlement:** Parties involved in sending or receiving a holding may opt to
  delay their
  :ref:`allocation <module-daml-finance-interface-settlement-v4-instruction-71097>` or
  :ref:`approval <module-daml-finance-interface-settlement-v4-instruction-71097>` of an instruction
  until just prior to the settlement time. This strategy prevents the
  :ref:`Batch <module-daml-finance-settlement-v4-batch-88124>` from settling prematurely.

- **Handling of Late Settlements:** To address cases where parties fail to settle by the desired
  time, we propose using a separate custom contract instance. This contact could facilitate the
  rolling of a :ref:`Batch <module-daml-finance-settlement-v4-batch-88124>` (along with its
  :ref:`Instruction <module-daml-finance-settlement-v4-instruction-73130>`\s) into a subsequent
  settlement cycle if the initial settlement period lapses. Additionally, the contract could allow
  for imposing penalties on parties that fail to allocate or approve the transaction in a timely
  manner.

For scenarios requiring a more stringent settlement process, customers are welcome to provide their
own implementation. This custom implementation can include specific checks and controls tailored to
their particular needs and requirements.

Remarks and further references
******************************

The settlement concepts are also explored in the
:doc:`Settlement tutorial <../tutorials/getting-started/settlement>`.
