.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Enhanced Transfers
##################

In this tutorial, we delve deeper into the concepts that were introduced in our getting-started
tutorials. In particular, we will extend on the :doc:`transfer <../getting-started/transfer>`
tutorial.

We begin by understanding the simple form of settlement. It transpires when a customer's funds are
transferred to another account within the same bank. Consequently, the sender's account is debited
(balance decreases), and the recipient's account is credited (balance increases). This process is
internally managed within the bank's systems and usually occurs instantly, as it doesn't require
interaction with external systems or institutions.

In Daml Finance, such fund transfers are not necessarily represented by a settlement workflow that
involves allocating and approving instructions. Instead, a "direct" transfer of funds can occur
between two parties, such as Alice and Bob. This transfer debits the sending account and atomically
credits the receiving account.

Next, we will explore how to configure the controllers responsible for authorizing incoming
transfers (credits) and outgoing transfers (debits) of holdings to an account.

Configuring Account Controllers
*******************************

The :ref:`Controllers <type-daml-finance-interface-account-v4-account-controllers-59817>` data type
specifies the parties that need to authorize incoming and outgoing transfers to an account.

For this tutorial, we provide four example scripts illustrating various incoming and outgoing
controller settings:

+--------------------------+--------------+----------------------------------+
| Script                   | Incoming     | Outgoing                         |
|                          | Controllers  | Controllers                      |
+==========================+==============+==================================+
| runDualControlTransfer   | Anyone       | Both (owner and custodian)       |
|                          |              |                                  |
| runDiscretionaryTransfer | Custodian    | Custodian                        |
|                          |              |                                  |
| runSovereignTransfer     | Owner        | Owner                            |
|                          |              |                                  |
| runUnilateralTransfer    | Anyone       | Owner                            |
+--------------------------+--------------+----------------------------------+

Each script begins by running a setup script ``runSetupTransferRequestWith`` that requests a
transfer of a holding from Alice to Bob at the Bank. The setup script takes a configuration as input
to set up Alice's and Bob's account controllers, as outlined in the table above.

The last step of the setup script creates a transfer request of a holding from Alice to Bob:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- TRANSFER_REQUEST_SETUP_BEGIN
  :end-before: -- TRANSFER_REQUEST_SETUP_END

The transfer ``Request`` template is designed for the stepwise collection of the necessary
authorizations for transferring a holding to a new owner:

.. literalinclude:: ../../finance-settlement/daml/Workflow/Transfer.daml
  :language: daml
  :start-after: -- TRANSFER_REQUEST_BEGIN
  :end-before: -- TRANSFER_REQUEST_END

Dual Control
============

In the ``runDualControlTransfer`` script, both the custodian and the owner of an account must
authorize outgoing transfers (debits), while incoming transfers (credits) require no authorization.

This script begins by setting up accounts accordingly and creating a transfer request instance:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- DUAL_CONTROL_SETUP_BEGIN
  :end-before: -- DUAL_CONTROL_SETUP_END

To execute the transfer, both the Bank and Alice must authorize:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- DUAL_CONTROL_TRANSFER_BEGIN
  :end-before: -- DUAL_CONTROL_TRANSFER_END

Discretionary
=============

The ``runDiscretionaryTransfer`` script specifies that the custodian controls both incoming and
outgoing transfers:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- DISCRETIONARY_SETUP_BEGIN
  :end-before: -- DISCRETIONARY_SETUP_END

Following the setup, the Bank can execute the transfer single-handedly:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- DISCRETIONARY_TRANSFER_BEGIN
  :end-before: -- DISCRETIONARY_TRANSFER_END

Sovereign
=========

In the ``runSovereignTransfer`` script, the owner controls both incoming and outgoing transfers:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- SOVEREIGN_SETUP_BEGIN
  :end-before: -- SOVEREIGN_SETUP_END

As Alice is the outgoing controller of the sending account, and Bob is the incoming controller of
the receiving account, both need to authorize the transfer:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- SOVEREIGN_TRANSFER_BEGIN
  :end-before: -- SOVEREIGN_TRANSFER_END

Unilateral
==========

In our final example script, `runUnilateralTransfer`, the owner controls outgoing transfers, while
incoming transfers require no additional authorization:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- UNILATERAL_SETUP_BEGIN
  :end-before: -- UNILATERAL_SETUP_END

Once the setup is complete, Alice can independently execute the transfer to Bob:

.. literalinclude:: ../../finance-settlement/daml/Scripts/Transfer.daml
  :language: daml
  :start-after: -- UNILATERAL_TRANSFER_BEGIN
  :end-before: -- UNILATERAL_TRANSFER_END

Summary
*******

By now, you should understand how to configure incoming and outgoing controllers for accounts based
on your requirements. Key concepts to remember include:

* To execute a transfer between a sender and a receiver, the outgoing controllers of the sending
  account and the incoming controllers of the receiving account need to authorize it.
* The required authorization can be provided by a generalized propose-accept template, which allows
  more than one party to accept.

Ownership transfers usually occur as part of a larger financial transaction. The next tutorials will
guide you on how to create such a transaction and how to settle it atomically.
