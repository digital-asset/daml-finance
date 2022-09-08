.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml Finance
############

Daml Finance is currently an `Early Access Feature in Alpha status <https://docs.daml.com/support/status-definitions.html>`_.

Introduction
************

Daml Finance is a library that supports customers modeling financial use-cases in Daml. It
comes with Daml packages that cover the following areas:

-  *Instruments*: modeling a set of financial instruments and their
   economic terms (that is, their rights and obligations)
-  *Holdings*: modeling relationships of ownership in a financial
   instrument (that is, clearly define who owns the rights, who has the
   obligations and the amount/quantity)
-  *Settlement*: atomic settlement of transactions involving holdings
-  *Lifecycling*: evolution of financial instruments over their lifetime
   based on their economic terms

The main goal of Daml Finance is to decrease time-to-market, and to increase productivity of development teams. Without it, every single customer would have to “reinvent the wheel” to produce the same models required in most financial use cases.

.. _clone-repo:

How to clone and build the repository
*************************************

If you want to have a more detailed look in the Daml Finance codebase, you can clone the repository
locally on your machine. That allows you to navigate the code, both the template definitions
and the tests.

The tests in particular are useful to show how the library works and how the different
components interact with each other.

As a pre-requisite, the `Daml SDK <https://docs.daml.com/getting-started/installation.html>`_ needs to be installed on your
machine.

In order to download `Daml Finance <https://github.com/digital-asset/daml-finance>`_, open a terminal and run:

.. code-block:: shell

   git clone git@github.com:digital-asset/daml-finance.git

This creates a new folder containing `Daml Finance <https://github.com/digital-asset/daml-finance>`_ .
Navigate to the folder and then run:

.. code-block:: shell

   make build

This downloads any required packages and builds the project.
You can then run:

.. code-block:: shell

   daml studio

to open the code editor and inspect the code.

Next steps
**********

The :doc:`Architecture <architecture>` page gives you an overview of the available packages and their API.

The :doc:`Core Concepts <core-concepts>` page defines the fundamental templates and interfaces provided by the library.

The :doc:`Getting Started <tutorial/getting-started/intro>` tutorials introduce the basic functionalities of the library through practical examples.
