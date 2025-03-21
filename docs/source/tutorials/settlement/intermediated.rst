.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Intermediated Settlement
########################

This tutorial expands upon the principles discussed in our previous
:doc:`Internal Settlement <internal>` tutorial, providing an in-depth exploration of intermediated
settlement involving a multi-level account hierarchy across different custodians.

Understanding Intermediated Settlement with Examples
****************************************************

This tutorial features two example scripts, ``runWrappedTransfersSettlement`` and
``runRouteProviderSettlement``, illustrating how to settle a batch with multiple instructions and
custodians.

Each script commences with ``runSetupIntermediatedSettlement``, initiating parties, establishing an
instrument issued by the Central Bank, and setting up an account hierarchy rooted at the Central
Bank. This hierarchy includes two custodian banks, Bank1 and Bank2, and their respective clients
Alice, Bob, and Charlie. Holdings are created for Alice@Bank1, Bank1@CentralBank, and Charlie@Bank2:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Intermediated.daml
  :language: daml
  :start-after: -- WRAPPED_TRANSFERS_SETUP_BEGIN
  :end-before: -- WRAPPED_TRANSFERS_SETUP_END

The below diagram illustrates the setup, where edges represent accounts and stars (``*``) denote
holdings:

.. code-block:: none

        Central Bank
        */       \
       Bank1    Bank2
      */   \   */   \
  Alice   Charlie   Bob

Wrapped Transfers: A Detailed Analysis
======================================

Our first example elucidates how two transfers at separate custodians can be consolidated into a
single batch. The routed steps are as follows:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Intermediated.daml
  :language: daml
  :start-after: -- WRAPPED_TRANSFERS_DISCOVER_BEGIN
  :end-before: -- WRAPPED_TRANSFERS_DISCOVER_END

These steps are converted into a batch and instructions:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Intermediated.daml
  :language: daml
  :start-after: -- WRAPPED_TRANSFERS_INSTRUCT_BEGIN
  :end-before: -- WRAPPED_TRANSFERS_INSTRUCT_END

The following diagram visualizes this pre- and post-settlement of the batch, where ``>`` signifies
settlement instructions, and stars (``*``) represents the holdings:

.. code-block:: none

        Central Bank                         Central Bank
        */       \                           */        \
      Bank1     Bank2        "Settle"       Bank1     Bank2
     */   \    */   \           =>          /   \*    /   \*
  Alice > Charlie > Bob                 Alice   Charlie   Bob
        T1        T2

Similar to the Internal Settlement tutorial, Alice allocates her Bank1 holding through a pledge,
while Bob approves its instruction by taking delivery at his account at Bank2. The intermediary,
Charlie, approves by taking delivery to his Bank1 account and allocates by pledging his pre-existing
holding at Bank2:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Intermediated.daml
  :language: daml
  :start-after: -- WRAPPED_TRANSFERS_SETTLE_BEGIN
  :end-before: -- WRAPPED_TRANSFERS_SETTLE_END

Important to note, Charlie cannot pass-through Alice's holding to Bob, as in the
:doc:`Internal Settlement <internal>` tutorial, due to the holdings having different custodians.
Therefore, for settlement to occur, Charlie needs a holding at Bank2. The settlement alters
Charlie's counterparty risk, shifting it from Bank1 to Bank2. This is a situation Charlie might
wish to avoid. The upcoming example shows how to involve a route provider, eliminating the need for
Charlie to hold any upfront holdings, thus preserving his counterparty exposure.

Route Provider: An Alternative Approach
=======================================

Our second example follows a similar setup to the first, involving a settlement step between Alice
and Charlie (S1), and another between Charlie and Bob (S2). However, these steps do not specify a
custodian, unlike the routed steps.

To convert the settlement steps S1 and S2 into routed steps, we engage a route provider. The
provider suggests preferable routes to the Central Bank for each client. During the "Discover"
action, each step is transformed into a sequence of routed steps

* S1 -> (T1)
* S2 -> (D, T2, C)

where T1 and T2 denote settlements by transfers, D represents a Debit, and C signifies a Credit. The
diagram below depicts the effects of the discovery and settlement process:

.. code-block:: none

                                       Central Bank                   Central Bank
                       "Discover"      */   T2   \       "Settle"      /       *\
                           =>         Bank1  >  Bank2       =>       Bank1  >  Bank2
                                      /  D^     /  C\                /   \     /  *\
  Alice > Charlie > Bob             */     \   /     v            Alice  Charlie   Bob
       S1         S2             Alice  >  Charlie   Bob
                                       T1

Let's begin with the "Discover" action:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Intermediated.daml
  :language: daml
  :start-after: -- ROUTE_PROVIDER_DISCOVER_BEGIN
  :end-before: -- ROUTE_PROVIDER_DISCOVER_END

Followed by the creation of the batch and instructions:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Intermediated.daml
  :language: daml
  :start-after: -- ROUTE_PROVIDER_INSTRUCT_BEGIN
  :end-before: -- ROUTE_PROVIDER_INSTRUCT_END

Finally, Alice, Charlie, Bank1, Bank2, and Bob allocate and approve their instructions accordingly:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Intermediated.daml
  :language: daml
  :start-after: -- ROUTE_PROVIDER_SETTLE_BEGIN
  :end-before: -- ROUTE_PROVIDER_SETTLE_END

Once the batch is settled, all instructions are executed atomically, causing a coordinated change in
the account hierarchy's holdings. Importantly, Charlie acted as an intermediary, providing a route
from Alice to Bob, without having to use any holdings upfront.

Summary
*******

You know how to define complex transactions and settle them atomically. Crucial points to remember
are:

* A route provider serves the purpose of discovering settlement routes or routed steps for each
  settlement step.
* Viewing the account hierarchy as a tree — with the Central Bank at the root, custodians on the
  second level, and clients as leaves — transfers occur on horizontally directed routed steps,
  debits on upwards directed routed steps, and credits on downward directed routed steps.

As a challenge for the curious reader, try extending these examples to settle two instruments using
settlement routes across two different account hierarchies.
