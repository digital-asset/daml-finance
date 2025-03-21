.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Getting Started
###############

This section explains how some key concepts of Daml Finance work in practice. It combines a step by
step description of different workflows with supporting Daml code.

The following tutorials are available:

* :doc:`Holdings <holdings>`: describes the core asset model used in Daml Finance.
* :doc:`Transfer <transfer>`: shows how to transfer ownership of a holding to another party.
* :doc:`Settlement <settlement>`: explains how to execute multiple asset movements atomically.
* :doc:`Lifecycling <lifecycling>`: describes how lifecycle rules and events can be used to evolve
  instruments over time.

Each tutorial builds on top of the previous ones, so they should ideally be followed in order.

Prerequisites
*************

We expect the reader to be familiar with the basic building blocks of Daml. If that is not the case,
a suitable introduction can be found `here <https://www.digitalasset.com/developers/learn>`_.

An understanding of :doc:`Daml Interfaces <../../../daml/reference/interfaces>` is very helpful, as
these are used extensively throughout the library. However, you should be able to follow along and
grasp the fundamental concepts also without detailed knowledge on interfaces.

Finally, make sure that the :doc:`Daml SDK <../../../getting-started/installation>`
is installed on your machine.

Download the Code for the Tutorials
***********************************

Open a new terminal window and run:

.. code-block:: shell

   daml new quickstart-finance --template quickstart-finance

This creates a new folder with contents from our template. Navigate to the folder and then run the
following to download the required Daml Finance packages:

.. code-block:: shell

   ./get-dependencies.sh

or, if you are using Windows

.. code-block:: shell

   ./get-dependencies.bat

Finally, you can start Daml Studio to inspect the code and run the project's scripts:

.. code-block:: shell

   daml studio

.. _structure-of-code-dependencies:

Structure of the Code and Dependencies
**************************************

The project includes

- four workflows defined in the ``Workflows`` folder
- four Daml scripts defined in the ``Scripts`` folder

The ``Workflows`` encapsulate the core business logic of the application, whereas the ``Scripts``
are meant to be executed on a one-off basis.

As you can see from the import list, modules in the ``Workflows`` folder depend only on
*interface* packages of Daml Finance (the packages that start with ``Daml.Finance.Interface.*``).

This is important, as it decouples the user-defined business logic from the template implementations
used in Daml Finance, which makes it easier to upgrade the application. The user-defined business
logic in the ``Workflows`` will not need to be modified nor re-compiled to work with
upgraded (ie., newer versions of) *implementation* packages.

On the other hand, modules in the ``Scripts`` folder depend on both the *interface* packages and
the *implementation* packages (in this case, ``Daml.Finance.Account.V4``, ``Daml.Finance.Holding.V4``,
and ``Daml.Finance.Instrument.Token.V4``). This is not problematic as scripts are meant to be run only
once when the application is initialized.