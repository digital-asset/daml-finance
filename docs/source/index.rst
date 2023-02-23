.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
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
#. Understand the :doc:`fundamental concepts <concepts/index>` in depth
#. Learn how to :doc:`model different financial instruments <tutorials/instrument-modeling/intro>`
#. Read up on the :doc:`background, purpose, and intended usage <overview/intro>` of the library
#. Explore the
   `Daml Finance Reference Application <https://github.com/digital-asset/daml-finance-app>`_

.. _releases:

Releases
********

This section details the list of released packages for each Daml SDK release. It also provides
status information for each package according to the
`Daml Ecosystem convention <https://docs.daml.com/support/status-definitions.html>`_.

Daml SDK 2.6.0
==============

Stable Packages
---------------

+--------------------------------------------+--------------------+--------+
| Package                                    | Version            | Status |
+============================================+====================+========+
| Daml.Finance.Account                       | 1.0.1              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Claims                        | 1.0.1              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Data                          | 1.0.1              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Holding                       | 1.0.2              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Generic            | 1.0.1              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Token              | 1.0.1              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Data                | 2.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Types.Date          | 2.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Lifecycle                     | 1.0.1              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Settlement                    | 1.0.2              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Util                          | 2.0.0              | Stable |
+--------------------------------------------+--------------------+--------+

Early Access Packages
---------------------

+--------------------------------------------+--------------------+--------+
| Package                                    | Version            | Status |
+============================================+====================+========+
| Daml.Finance.Instrument.Bond               | 0.2.1              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Equity             | 0.2.1              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Option             | 0.1.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Swap               | 0.2.1              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Bond     | 0.2.1              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Option   | 0.1.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Swap     | 0.2.1              | Alpha  |
+--------------------------------------------+--------------------+--------+

Deprecated Packages
-------------------

+--------------------------------------------+--------------------+--------+
| Package                                    | Version            | Status |
+============================================+====================+========+
| Daml.Finance.Interface.Data                | 1.*                | Depr.  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Types.Date          | 1.*                | Depr.  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Util                          | 1.*                | Depr.  |
+--------------------------------------------+--------------------+--------+

Daml SDK 2.5.0
==============

Stable Packages
---------------

+--------------------------------------------+--------------------+--------+
| Package                                    | Version            | Status |
+============================================+====================+========+
| ContingentClaims.Core                      | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| ContingentClaims.Lifecycle                 | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Account                       | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Claims                        | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Data                          | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Holding                       | 1.0.1              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Generic            | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Token              | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Account             | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Claims              | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Data                | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Holding             | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Base     | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Generic  | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Token    | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Lifecycle           | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Settlement          | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Types.Common        | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Types.Date          | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Util                | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Lifecycle                     | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Settlement                    | 1.0.1              | Stable |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Util                          | 1.0.0              | Stable |
+--------------------------------------------+--------------------+--------+

Early Access Packages
---------------------

+--------------------------------------------+--------------------+--------+
| Package                                    | Version            | Status |
+============================================+====================+========+
| ContingentClaims.Valuation                 | 0.2.0              | Labs   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Bond               | 0.2.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Equity             | 0.2.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Option             | 0.1.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Swap               | 0.2.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Bond     | 0.2.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Equity   | 0.2.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Option   | 0.1.0              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Swap     | 0.2.0              | Alpha  |
+--------------------------------------------+--------------------+--------+

Deprecated Packages
-------------------

+--------------------------------------------+--------------------+--------+
| Package                                    | Version            | Status |
+============================================+====================+========+
| None                                                                     |
+--------------------------------------------+--------------------+--------+


.. toctree::
   :maxdepth: 0
   :hidden:

   self
   overview/index
   concepts/index
   packages/index
   tutorials/index
   reference/index
