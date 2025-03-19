.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml Finance Documentation
##########################

Welcome to the Daml Finance documentation. This page provides an overview of the documentation
content as well as suggested starting points. Use the left-hand menu to explore the various
sections, or the search bar above for quick navigation. If you are missing content from the
documentation, have feedback on the library, or need any help using it, do not hesitate to
`open an issue <https://github.com/digital-asset/daml-finance/issues>`_ on the repository.

Content
*******

* :doc:`Overview <overview/intro>`: description of the purpose of the library, its high-level
  architecture, as well as targeted use cases
* :doc:`Concepts <concepts/index>`: explanation of the main concepts used throughout the library,
  and how they fit together
* :doc:`Instruments <instruments/intro>`: description of the instruments that are
  included in Daml Finance and can be used out of the box
* :doc:`Packages <packages/index>`: documentation for each individual package and its contained
  modules
* :doc:`Tutorials <tutorials/getting-started/intro>`: step-by-step implementation guides across
  different use cases
* :doc:`Reference <reference/glossary>`: glossary as well as code-level documentation for each
  package

Starting Points
***************

The following is a suggested learning path to get productive quickly:

#. :doc:`Get started <tutorials/getting-started/intro>` quickly
#. Read up on the :doc:`background, purpose, and intended usage <overview/intro>` of the library
#. Understand the :doc:`fundamental concepts <concepts/index>` in depth
#. Learn how to
   :doc:`use the instrument packages to model different financial instruments <instruments/intro>`
#. Explore the :ref:`Daml Finance Demo Application <daml-finance-demo-app>`
#. Ask questions in the `Forum <https://discuss.daml.com/tag/daml-finance>`_ or read existing
   discussions

.. _releases:

Packages
********

How to Download
===============

Daml Finance is distributed as a set of packages. There are two main ways how to download Daml
Finance:

- New users are recommended to follow the
  :doc:`Getting started <tutorials/getting-started/intro>` tutorial, which also contains a
  ``get-dependencies`` script that downloads the Daml Finance packages. This allows you to learn
  Daml Finance at the same time.
- Advanced users can download Daml Finance directly from the
  `Releases section of the repo <https://github.com/digital-asset/daml-finance/releases>`_, either
  the individual packages that your application needs or a bundle containing all packages.

Current Release
===============

Daml SDK 2.10.0

This section details the list of released and deprecated packages, with status information provided
for each package according to the
`Daml Ecosystem convention <https://docs.daml.com/support/status-definitions.html>`_. Additionally,
a section is included to highlight and explain the major updates and enhancements introduced since
the last release.

**Important Note**: The current Daml Finance release was built with Daml SDK 2.10.0 and Daml LF
version 1.17 (LF 1.17) to enable Smart Contract Upgrading (SCU). To use this release, ensure your
project is using Daml SDK 2.10.0 or later and LF 1.17. By default, SDK 2.10.0 uses LF 1.15, but you
can enable LF 1.17 by adding `--target=1.17` as `build-options` in your daml.yaml file.

Smart Contract Upgradeability (SCU)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Daml Finance is now SCU compatible by relying on Daml SDK 2.10.0 and Daml LF 1.17. As part of this
change, each package incorporates its major version number (Vx) into its path, package name, and
module name. Consequently, the major version for all Daml Finance packages has been incremented.

Context-Aware Semaphore Lock Release
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A fix added to the Daml.Finance.Util package ensures that a holding protected by a semaphore lock
will only be released if the lock's context matches the provided unlock context. This prevents
inadvertent releases.

New AutoCallable Instrument
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A new AutoCallable instrument
:ref:`AutoCallable <module-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-68544>`
has been added to the experimental
Daml.Finance.Instrument.StructuredProduct.V0 package. This addition expands the set of available
structured product instruments.

Stable Packages
---------------

+----------------------------------------------+---------+--------+
| Package                                      | Version | Status |
+==============================================+=========+========+
| ContingentClaims.Core.V3                     | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| ContingentClaims.Lifecycle.V3                | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Account.V4                      | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Claims.V3                       | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Data.V4                         | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Holding.V4                      | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Instrument.Bond.V3              | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Instrument.Generic.V4           | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Instrument.Token.V4             | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Account.V4            | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Claims.V4             | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Data.V4               | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Holding.V4            | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.Base.V4    | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.Bond.V3    | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.Generic.V4 | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.Token.V4   | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.Types.V2   | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Lifecycle.V4          | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Settlement.V4         | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Types.Common.V3       | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Types.Date.V3         | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Interface.Util.V3               | 3.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Lifecycle.V4                    | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Settlement.V4                   | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+
| Daml.Finance.Util.V4                         | 4.0.0   | Stable |
+----------------------------------------------+---------+--------+

Early Access Packages
---------------------

+--------------------------------------------------------+---------+--------+
| Package                                                | Version | Status |
+========================================================+=========+========+
| ContingentClaims.Valuation.V0                          | 0.3.0   | Labs   |
+--------------------------------------------------------+---------+--------+
| Daml.Finance.Instrument.Equity.V0                      | 0.5.0   | Alpha  |
+--------------------------------------------------------+---------+--------+
| Daml.Finance.Instrument.Option.V0                      | 0.4.0   | Alpha  |
+--------------------------------------------------------+---------+--------+
| Daml.Finance.Instrument.StructuredProduct.V0           | 0.2.0   | Alpha  |
+--------------------------------------------------------+---------+--------+
| Daml.Finance.Instrument.Swap.V0                        | 0.5.0   | Alpha  |
+--------------------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.Equity.V0            | 0.5.0   | Alpha  |
+--------------------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.Option.V0            | 0.4.0   | Alpha  |
+--------------------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.StructuredProduct.V0 | 0.2.0   | Alpha  |
+--------------------------------------------------------+---------+--------+
| Daml.Finance.Interface.Instrument.Swap.V0              | 0.5.0   | Alpha  |
+--------------------------------------------------------+---------+--------+

Deprecated Packages
-------------------

+-----------------------------------------------+--------------------+--------+
| Package                                       | Version            | Status |
+===============================================+====================+========+
| ContingentClaims.Core                         | 2.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| ContingentClaims.Lifecycle                    | 2.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Account                          | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Claims                           | 2.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Data                             | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Holding                          | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Generic               | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Token                 | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Account                | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Claims                 | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Data                   | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Holding                | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Base        | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Generic     | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Token       | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Lifecycle              | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Settlement             | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Util                   | 2.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Lifecycle                        | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Settlement                       | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+
| Daml.Finance.Util                             | 3.*                | Depr.  |
+-----------------------------------------------+--------------------+--------+

