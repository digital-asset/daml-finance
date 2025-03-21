.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Transfer
########

This tutorial builds on the previous chapter, which introduced :ref:`account <account>`,
:ref:`instrument <instrument>`, and :ref:`holding <holding>`.

We are now going to transfer the holding that we created in the previous tutorial
from Alice to Bob.

Run the Script
**************

Let us now explore the ``Transfer`` script step-by-step. It builds on the previous
:doc:`Holdings <holdings>` tutorial script in the sense that the same accounts and the existing
holdings are used.

Transfer Cash from Alice to Bob
===============================

The final step of our ``Setup`` script transfers Alice’s holding to Bob using the ``Transfer``
workflow. In our tutorial example, the receiver of the cash makes the transfer request:

.. literalinclude:: ../../quickstart-finance/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- TRANSFER_BEGIN
  :end-before: -- TRANSFER_END

Bob requests the cash to be transferred to his account. Alice then accepts the request.


Frequently Asked Questions
**************************

How does the ``Transfer`` workflow work?
========================================

If you look at the implementation of the ``Transfer`` workflow, you will notice the following lines:

.. literalinclude:: ../../quickstart-finance/daml/Workflow/Transfer.daml
  :language: daml
  :start-after: -- DO_TRANSFER_BEGIN
  :end-before: -- DO_TRANSFER_END

The first line converts the holding contract id (of type
:ref:`ContractId Holding.I <module-daml-finance-interface-holding-v4-holding-20535>`) to the
:ref:`Transferable.I <module-daml-finance-interface-holding-v4-transferable-93054>`
interface using ``coerceInterfaceContractId``.

Then, the ``Transfer`` choice, defined as part of the
:ref:`Transferable <module-daml-finance-interface-holding-v4-transferable-93054>`
interface, is exercised.

Finally, the new holding is converted back to a
:ref:`Holding.I <module-daml-finance-interface-holding-v4-holding-20535>`
before it is returned. This is done using ``toInterfaceContractId``.

In order to fully understand these instructions, we need to keep in mind the interface hierarchy
used by our holding implementation.

.. image:: ../../images/interface_hierarchy.png
  :alt: A diagram of the interface hierarchy. From left to right, Disclosure, Lockable, Holding,
        and Transferable are each linked by arrows pointing left. Additionally, Fungible is located
        above from Transferable, and has an arrow pointing to Holding. All arrows are labeled
        "requires".

We use ``coerceInterfaceContractId`` to convert the
:ref:`Holding.I <module-daml-finance-interface-holding-v4-holding-20535>`
to a :ref:`Transferable <module-daml-finance-interface-holding-v4-transferable-93054>`.
The success of this operation is not guaranteed and will result in a run-time error if the holding
implementation at hand does not implement
:ref:`Transferable <module-daml-finance-interface-holding-v4-transferable-93054>`.

We use ``toInterfaceContractId`` to convert back to a
:ref:`Holding <module-daml-finance-interface-holding-v4-holding-20535>`.
This is because all
:ref:`Transferable <module-daml-finance-interface-holding-v4-transferable-93054>`\ s
implement the :ref:`Holding.I <module-daml-finance-interface-holding-v4-holding-20535>`
interface, so the validity of this operation is guaranteed at compile-time.

Why is Alice an observer on Bob’s account?
==========================================

You might have noticed that Alice is an observer of Bob’s account and you might be wondering why
this is the case.

This is because the party exercising the ``Transfer`` choice, which in this case is Alice, needs to
fetch Bob’s account in order to verify that it has not been archived.

If we wanted to avoid Bob’s account contract ever being disclosed to Alice, we would need a third
party (in this case the Bank) to execute the ``Transfer``.

Exercises
*********

There are a couple of improvements to the code that can be implemented as an exercise. They will
help you familiarize yourself with the library and with Daml interfaces.

Split the Holding to Transfer the Right Amount
==============================================

In the example, Bob requests ``USD 1000`` from Alice and Alice allocates a holding for exactly the
right amount, because the transfer would otherwise fail. We want the transfer to be successful also
if Alice allocates a holding for a larger amount e.g., ``USD 1500``.

We can leverage the fact that the holding implements the
:ref:`Fungible <module-daml-finance-interface-holding-v4-fungible-55495>`
interface, which makes it possible to ``Split`` it into a holding of ``USD 1000`` and one of
``USD 500``. In the implementation of the ``CashTransferRequest_Accept`` choice:

- cast the allocated holding to the :ref:`Fungible
  <module-daml-finance-interface-holding-v4-fungible-55495>` interface
- use the ``Split`` choice to split the larger holding into two holdings
- execute the transfer, allocating the holding with the correct amount

In the last step, you will need to cast the
:ref:`Fungible <module-daml-finance-interface-holding-v4-fungible-55495>` to a
:ref:`Transferable <module-daml-finance-interface-holding-v4-transferable-93054>`
using ``toInterfaceContractId``.

Temporary Account Disclosure
============================

There is no reason for Alice to be an observer on Bob's account before the transfer is initiated by
Bob (and after the transfer is executed).

Modify the original code, such that:

- Bob's account is disclosed to Alice once the transfer is initiated
- When the Transfer is executed, Alice removes herself from the account observers

In order to do that, you can leverage the fact that
:ref:`Account <module-daml-finance-account-v4-account-5834>`
implements the
:ref:`Disclosure <module-daml-finance-interface-util-v3-disclosure-50779>`
interface. This interface exposes the ``AddObservers`` and ``RemoveObservers`` choices, which can be
used to disclose / undisclose Bob's account contract to Alice. In order to exercise these choices,
you can use the :ref:`Account.exerciseInterfaceByKey
<function-daml-finance-interface-account-v4-account-exerciseinterfacebykey-87310>` utility function.

Summary
*******

You now learned how to perform a simple transfer. The key concepts to take away are:

* Holdings represent the ownership of a financial instrument at a custodian.
* Transfers change ownership of a holding.

Ownership transfers typically happen as part of a larger financial transaction. The next tutorial
will show you how to create such a transaction and how to settle it atomically.
