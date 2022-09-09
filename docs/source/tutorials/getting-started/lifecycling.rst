.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Lifecycling
###########

This tutorial introduces the :ref:`lifecycling <lifecycling>` framework of the library with a simple example.
The purpose is to demonstrate how lifecycle rules and events can be used to process a dividend payment.

We are going to

#. create a new version of the token instrument
#. create the required lifecycle rules
#. create a distribution event
#. process the event to produce the effects from the distribution
#. instruct settlement by presenting a token holding
#. settling the resulting batch atomically

This example builds on the previous :doc:`Settlement <settlement>` tutorial script such that the same
accounts and existing holdings can be used.

Overview of the lifecycle process
*********************************

We first give a high-level outline of the lifecycle process.

+-------------------------------------------------+-----------------------------------------------------------------------------------------+
| 1. Create a lifecycle rule                      | A lifecycle rule implements the logic to calculate effects for a given lifecycle event. |
|                                                 | In our example we create a distribution rule to handle the dividend event on our token. |
|                                                 |                                                                                         |
+-------------------------------------------------+-----------------------------------------------------------------------------------------+
| 2. Create a lifecycle event                     | The lifecycle event refers to the *target instrument* the event applies to. Holdings on |
|                                                 | this instrument can then be used to claim the resulting lifecycle effect.               |
|                                                 |                                                                                         |
+-------------------------------------------------+-----------------------------------------------------------------------------------------+
| 4. Process the event through the lifecycle rule | The lifecycle rule contains the business logic to derive the lifecycle effects          |
|                                                 | resulting from a concrete event. The effect describes the per-unit holding transfers    |
|                                                 | that are to be settled between a custodian and the owner of a holding.                  |
|                                                 |                                                                                         |
+-------------------------------------------------+-----------------------------------------------------------------------------------------+
| 4. Claim the effect using a holding             | The claim rule is used to claim the effects resulting from a lifecycle event using a    |
|                                                 | holding on the target instrument. The result is a set of settlement instructions and    |
|                                                 | corresponding batch to be settled between the custodian and owner of the holding        |
|                                                 |                                                                                         |
+-------------------------------------------------+-----------------------------------------------------------------------------------------+

Running the script
******************

The code for this tutorial can be executed via the ``runLifecycling`` function in the ``Lifecycling.daml`` module.

The first part executes the script from the previous :doc:`Settlement <settlement>` tutorial to arrive at the initial state for this scenario.
We then create a new version of the *token* instrument, which is required for defining the distribution event. A clock is created as a required
input to the distribution rule, to determine if the event is due to be processed.

Next, we create two lifecycle rule:

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Lifecycling.daml
  :language: daml
  :start-after: -- LIFECYCLE_RULES_BEGIN
  :end-before: -- LIFECYCLE_RULES_END

* The :ref:`Distribution Rule <type-daml-finance-interface-settlement-batch-batch-97497>` defines the business logic to calculate the resulting
  lifecycle effect from a given distribution event. It is signed by the `Bank` as a provider.
* The :ref:`Claim Rule <type-daml-finance-interface-settlement-batch-batch-97497>` allows a holder of the target instrument to claim the effect
  resulting from the distribution event. By presenting their holding they can instruct the settlement of the holding transfers described in the effect.

Then we create a distribution event describing the terms of the dividend to be payed.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Lifecycling.daml
  :language: daml
  :start-after: -- CREATE_EVENT_BEGIN
  :end-before: -- CREATE_EVENT_END

We can now process the distribution event using the distribution rule.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Lifecycling.daml
  :language: daml
  :start-after: -- LIFECYCLE_EVENT_BEGIN
  :end-before: -- LIFECYCLE_EVENT_END

The result of this is an effect describing the per-unit asset movements to be executed for token holders. Each holder can now present their holding
to *claim* the effect and instruct settlement of the associated entitlements.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Lifecycling.daml
  :language: daml
  :start-after: -- CLAIM_EVENT_BEGIN
  :end-before: -- CLAIM_EVENT_END

As a side-effect of claiming the effect the presented holding is exchanged for the new token version. This is to prevent a holder from claiming a
given effect twice.

In our example of a cash dividend only a single instruction is generated: the movement of cash from the bank to
the token holder. This instruction along with its batch is now settled the usual way, as described in the previous :doc:`Settlement <settlement>` tutorial.

.. literalinclude:: ../../../code-samples/getting-started/daml/Scripts/Lifecycling.daml
  :language: daml
  :start-after: -- EFFECT_SETTLEMENT_BEGIN
  :end-before: -- EFFECT_SETTLEMENT_END

Note that the bank in this case does not actually transfer the cash from another account, but simply credits Bob's account by using the ``CreditReceiver``
allocation type. In a real-world dividend scenario one would additionally model the flow of funds from the issuer to the bank using the same lifecycle
process as described above.

Frequently Asked Questions
**************************

Which party should create and sign the lifecycle rules and events?
==================================================================

In the simplified scenario for this tutorial we have used the bank as both the *issuer* and *depository* for the instruments involved. In a real-world case
instruments and their corresponding lifecycle rules and events would be maintained by an actual issuer, with the depository acting as a 3rd-party trust anchor.

Which parties typically take which actions in the lifecycle workflow?
=====================================================================

The lifecycle interfaces governing the process leave the controllers of the various choices in the process up to the implementation.
* Typically, we would expect the issuer of an instrument to be responsible to generate lifecycle events (for example, announcing dividends or stock splits).
* Lifecycle rules on the other hand are often controlled by 3rd-party calculation agents.
* The claiming of lifecycle effects in by default the responsability of the owner of a holding. If instead the owner wants to delegate this responsability to their custodian they can do so via a delegation contract.
* The party executing settlement can be chosen as well, as described in the previous tutorial on :doc:`Settlement <settlement>`.

Can an instrument act as its own lifecycle rule?
================================================

Yes, an instrument can implement the ``Lifecycle`` interface directly such that the lifecycle rules are contained within the instrument itself. An example
for this can be found in the implementation for generic instruments. There are, however, advantages to separating this logic out into rule contracts:
* Keeping lifecycle rules in a different package from your instruments allows you to independently upgrade or patch them without affecting your live
instruments.
* Having separate rules allows to change the lifecycle properties of an instrument dynamically at runtime. For example, an instrument can initially be created
without support for doing asset distributions. Then, at a later point the issuer might decide to start paying dividends. They can now simply add a
distribution rule to the running system to enables this new lifecycle event for their instrument without affecting the actual live instrument itself (and
therefore all the holdings on it)

Summary
*******

You have now learned how to use lifecycle rules and events to describe the behavior of an instrument. The key concepts to take away are:
* Lifecycle events represent different ways of how an instrument can evolve
* A lifecycle rule contains logic to calculate the effects an event has on an instrument and its holdings.
* A claim rule is used to instruct settlement for a given effect using a holding
