.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Lifecycling
###########

This section explains how to lifecycle instruments in Daml Finance. Each tutorial combines a step by
step description of different workflows with supporting code.

The following tutorials are available:

* The :doc:`Time-based lifecycling <fixed-rate-bond>` tutorial uses a fixed rate bond as an example
  to demonstrate time-based lifecycling.

* The :doc:`Observations <floating-rate-bond>` tutorial uses a floating rate bond as a sample
  instrument to show how ``Observations`` work. This applies to instruments whose payoff depends
  on an underlying asset.

* The :doc:`Election-based lifecycling <callable-bond>` tutorial uses a callable bond to
  explain how to create and process elections. This applies to instruments that require an
  active choice by one of the stakeholders.

Each tutorial builds on top of the previous ones, so they should ideally be followed in order.

Download the Code for the Tutorials
***********************************

As a prerequisite, make sure that the :doc:`Daml SDK <../../../getting-started/installation>`
is installed on your machine.

Open a terminal and run:

.. code-block:: shell

   daml new finance-lifecycling --template=finance-lifecycling

This creates a new folder with contents from our template. Navigate to the ``finance-lifecycling``
folder and then run the following to download the required Daml Finance packages:

.. code-block:: shell

   ./get-dependencies.sh

or, if you are using Windows

.. code-block:: shell

   ./get-dependencies.bat

Finally, you can start Daml Studio to inspect the code and run the project's scripts:

.. code-block:: shell

   daml studio