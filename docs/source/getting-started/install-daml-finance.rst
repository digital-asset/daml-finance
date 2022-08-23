.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Getting started : Installing Daml Finance
#########################################

If you want to have a more detailed look in the Daml Finance codebase, you can install the repo
locally on your machine. That allows you to navigate the code, both the template definitions
and the tests.

The tests in particular are useful to show how the library works and how the different
components interact with each other.

As a pre-requisite, the `Daml SDK <https://docs.daml.com/getting-started/installation.html>`_ needs to be installed on your
machine.

In order to install `Daml Finance <https://github.com/digital-asset/daml-finance>`_, open a terminal and run:

.. code-block:: shell

   git clone git@github.com:digital-asset/daml-finance.git

This creates a new folder containing `Daml Finance <https://github.com/digital-asset/daml-finance>`_ .
Navigate to the folder and then run

.. code-block:: shell

   make

to download any packages that are required and then build the project.
You can then run

.. code-block:: shell

   daml studio

to open the code editor and inspect the code.
