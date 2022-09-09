.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Introduction
############

Purpose
*******

Daml Finance supports the modeling of financial and non-financial use-cases in Daml. It provides
a standard way to represent assets on Daml ledgers and defines common behaviours and rules.
There are two main benefits from using the library in your application:

* Shortened time-to-market

   Implementing basic financial concepts like ownership or economic terms of an asset is a complex
   and tedious task. By providing common building blocks Daml Finance increases delivery velocity
   and shortens the time-to-market when building Daml applications. The rich set of functionality of
   Daml Finance is at your disposal so you don't have to reinvent the wheel.

* Application composability

   Building your application on Daml Finance makes it compatible with other platforms in the
   wider ecosystem. By using a shared library assets become "mobile" allowing them to be used seamlessly
   across application boundaries without the need for translation or integration layers. A
   Daml Finance-based asset that is, for example, originated in a bond issuance application can be used
   in the context of a secondary market trading application that is also built on Daml Finance.

Design Goals
************

Daml Finance optimizes for the following aspects:

* Accessibility
   The library is designed to have a low barrier for entry. Users familiar with Daml can get started
   quickly and can leverage the provided functionality easily.

* Maintainability
   Building with Daml Finance decouples your application code from the underlying representation of assets.
   This allows for the application to evolve without the need to migrate assets from one version to another.
   The application is decoupled from the assets such that maintenance becomes easier.

* Extensibility
   Various extension points allow for customization and extension of the library as required. If a provided
   implementation does not fulfil the requirements it is straigh-forward to provide a custom extension.

Scope
*****

The library covers the following areas:

* Holdings (ownership)
   Modeling of ownership structures, custodial relationships, intermediated securities, and accounts
* Instruments (behavior)
   Structuring the economic terms of an asset and the events that govern its evolution
* Settlement (movements)
   Executing complex transactions involving multiple parties and assets
* Lifecycle (evolution)
   Governing the lifecycle of financial instruments over their lifetime

Use cases
*********

Daml Finance comes with broad asset and workflow capabilities to allow for a variety of use cases to be modeled:

* Simple tokens: digital representation of traditional assets
* Central bank digital currency: retail or wholesale distribution models
* Standard asset classes: equities with corporate actions, bonds with flexible cashflow modeling
* Complex assets: time- and path-dependent derivatives with optionality
* Synchronized lifecycling: atomic, intermediated lifecycling and settlement of cash flows across investors and custodians
* Cross-entity issuance: atomic, multi-party issuance across investors, issuer, risk book, treasury
* Asset-agnostic trading facility: generic delivery-vs-payment and immediate, guaranteed settlement
* Exotic asset types: non-fungible and non-transferable assets

.. _explore-library:

Exploring the library
*********************

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
