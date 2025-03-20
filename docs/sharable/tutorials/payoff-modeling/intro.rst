.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Payoff Modeling
###############

This section contains an introduction to the Daml Finance
:doc:`Generic Instrument <../../instruments/generic>`, which provides a flexible framework to
structure custom payoffs and lifecycle them on the ledger.

The Generic Instrument encapsulates the
:doc:`Contingent Claims <../../instruments/generic/contingent-claims>` library, which models the
economic terms of an instrument based on its future cashflows and other contractual events.

The tutorials introduce the Contingent Claims modeling framework in a practical way and give you
the tools to

- structure financial instruments such as bonds, swaps, options, and other derivatives
- lifecycle the instruments on-ledger to calculate pending payments

The following tutorials are available:

* The :doc:`Basic Builders <basic-builders>` tutorial introduces the basic Contingent Claims
  builders.

* The :doc:`Observations <observations>` tutorial shows how to model market observables, such as
  interest rates or equity spot prices.

Download the Code for the Tutorials
***********************************

As a prerequisite, make sure that the :doc:`Daml SDK <../../../getting-started/installation>`
is installed on your machine.

Open a terminal and run:

.. code-block:: shell

   daml new finance-payoff-modeling --template=finance-payoff-modeling

This creates a new folder with contents from our template. Navigate to the
``finance-payoff-modeling`` folder and then run the following to download the required
Daml Finance packages:

.. code-block:: shell

   ./get-dependencies.sh

or, if you are using Windows

.. code-block:: shell

   ./get-dependencies.bat

Finally, you can start Daml Studio to inspect the code and run the project's scripts:

.. code-block:: shell

   daml studio

.. TODO explain the role of acquisition date in the observations section (together with path-dependent payoffs)
.. TODO Elections (options, callable bonds)
.. TODO Path dependent payoffs (credit default swaps, barrier options)
.. TODO Give some details on the internals of the script (mapping to time, mapping to actual instruments)
