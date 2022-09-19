.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Getting Started
###############

.. toctree::
   :hidden:

   transfer
   settlement
   lifecycling

This section explains how some key concepts of Daml Finance work in practice.
It combines a step by step description of different workflows with supporting code.

To follow the tutorials you can install the quickstart project via the Daml assistant by
executing the following command:

.. code-block:: shell

   daml new quickstart-finance --template=quickstart-finance
   cd quickstart-finance
   ./get-dependencies.sh
   daml studio

The following tutorials are available:

* :doc:`Transfer <transfer>`: describes accounts, cash instrument, deposits and transfers.
* :doc:`Settlement <settlement>`: describes how to execute multiple asset movements atomically.
* :doc:`Lifecycling <lifecycling>`: describes how lifecycle rules and events can be used to evolve instruments over time.
