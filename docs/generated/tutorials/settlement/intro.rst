.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Settlement
##########

This section explains how the settlement processes of Daml Finance work in detail.
It combines a step by step description of different workflows with supporting code.

The following tutorials are available:

* The :doc:`Enhanced Transfers <transfer>` tutorial builds upon the
  basic :doc:`Transfer <../getting-started/transfer>` tutorial from the Getting Started section.
  Specifically, we explore how to configure the controllers that need to authorize incoming
  transfers (credits) and outgoing transfers (debits) to and from an account, respectively.

* The :doc:`Internal Settlement <internal>` tutorial, an extension of the
  basic :doc:`Settlement <../getting-started/settlement>` Getting Started tutorial, illustrates how
  holdings can be transferred within a single custodian through a settlement workflow involving
  batches and related instructions. The allocation process of such instructions involves methods for
  committing a pre-existing holding (``Pledge``), a newly created holding (``CreditReceiver``), and
  a holding received simultaneously (``PassThroughFrom``). The approval methods include taking
  delivery of a holding to an account (``TakeDelivery``), immediately nullifying the holding
  (``DebitSender``), and passing the holding through (as allocation) to another instruction
  (``PassThroughTo``).

* The :doc:`Intermediated Settlement <internal>` tutorial builds upon the
  :doc:`Internal Settlement <internal>` tutorial and shows how to make use of a
  ``RouteProvider`` to settle instructions across account hierarchies involving more than one
  custodian.

Download the Code for the Tutorials
***********************************

As a prerequisite, make sure that the :doc:`Daml SDK <../../../getting-started/installation>`
is installed on your machine.

Open a terminal and run:

.. code-block:: shell

   daml new finance-settlement --template=finance-settlement

This creates a new folder with contents from our template. Navigate to the ``finance-settlement``
folder and then run the following to download the required Daml Finance packages:

.. code-block:: shell

   ./get-dependencies.sh

or, if you are using Windows

.. code-block:: shell

   ./get-dependencies.bat

Finally, you can start Daml Studio to inspect the code and run the project's scripts:

.. code-block:: shell

   daml studio
