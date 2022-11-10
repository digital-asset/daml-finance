.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Getting Started
###############

.. toctree::
   :hidden:

   transfer
   settlement
   lifecycling

Quickstart
**********

This section explains how some key concepts of Daml Finance work in practice.
It combines a step by step description of different workflows with supporting code.

To follow the tutorials you can install the quickstart project via the Daml assistant by
executing the following command:

.. code-block:: shell

   daml new quickstart-finance --template=quickstart-finance
   cd quickstart-finance
   ./get-dependencies.sh
   daml studio

Reference App
*************

In addition to Daml Finance, there also a separate Daml Finance Reference App.
It showcases several of the Daml Finance capabilites in a web-based graphical user interface.

If you are interested in trying out the Daml Finance Reference App locally, you can clone the corresponding repo
and follow the installation instructions on the `Daml Finance Reference App GitHub page <https://github.com/digital-asset/daml-finance-app>`_.


Create your own App
*******************

It is possible to generate *JavaScript* code from the Daml Finance packages you need, by running
daml with the `js <https://docs.daml.com/app-dev/bindings-ts/daml2js.html>`_ codegen option, for example:

.. code-block:: shell

   daml codegen js -o ./output daml-finance-interface-swap-0.1.7.dar daml-finance-interface-instrument-bond-0.1.7.dar

Alternatively, if your app uses *Java*, you can also run the `java <https://docs.daml.com/app-dev/bindings-java/index.html>`_ codegen in a similar way:

.. code-block:: shell

   daml codegen java -o ./output daml-finance-interface-swap-0.1.7.dar daml-finance-interface-instrument-bond-0.1.7.dar

Note, this Daml Finance codegen is only supported on SDK versions 2.5.x and higher.


Next Steps
**********

The following tutorials are available:

* :doc:`Transfer <transfer>`: describes accounts, cash instrument, deposits and transfers.
* :doc:`Settlement <settlement>`: describes how to execute multiple asset movements atomically.
* :doc:`Lifecycling <lifecycling>`: describes how lifecycle rules and events can be used to evolve instruments over time.
