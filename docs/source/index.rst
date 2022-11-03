.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml Finance Documentation
##########################

Welcome to the Daml Finance documentation. Daml Finance is currently an `Early Access Feature <https://docs.daml.com/support/status-definitions.html>`_ in Alpha status.
This page provides an overview of the documentation content as well as suggested starting points. Use the left-hand menu to explore the various sections, or the search bar above for quick navigation.
If you are missing content from the documentation, have feedback on the library, or need any help using it, do not hesitate to `open an issue <https://github.com/digital-asset/daml-finance/issues>`_ on the repository.

Content
*******

* :doc:`Overview <overview/intro>`: description of the purpose of the library, its high-level architecture, as well as targeted use cases.
* :doc:`Concepts <concepts/index>`: explanation of the main concepts used throughout the library, and how they fit together
* :doc:`Packages <packages/intro>`: documentation for each individual package and its contained modules
* :doc:`Tutorials <tutorials/getting-started/intro>`: step-by-step implementation guides across different use cases
* :doc:`Reference <reference/glossary>`: glossary as well as code-level documentation for each package

Starting Points
***************

The following is a suggested learning path to get productive quickly:

#. :doc:`Get started <tutorials/getting-started/intro>` quickly
#. Understand the :doc:`fundamental concepts <concepts/index>` in depth
#. Learn how to :doc:`model different financial instruments <tutorials/instrument-modeling/intro>`
#. Read up on the :doc:`background, purpose, and intended usage <overview/intro>` of the library
#. Explore the `Daml Finance Reference Application <https://github.com/digital-asset/daml-finance-app>`_

Package Status
**************

The following table lists the maturity status for each package contained within Daml Finance.
Status definitions follow the `Daml Ecosystem convention <https://docs.daml.com/support/status-definitions.html>`_.

+--------------------------------------------+--------------------+--------+
| Package                                    | Version            | Status |
+============================================+====================+========+
| ContingentClaims.Core                      | 3.0.0.20221026.1   | Beta   |
+--------------------------------------------+--------------------+--------+
| ContingentClaims.Lifecycle                 | 3.0.0.20221026.1   | Beta   |
+--------------------------------------------+--------------------+--------+
| ContingentClaims.Valuation                 | 3.0.0.20221026.1   | Labs   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Account                       | 0.1.1              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Claims                        | 0.1.1              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Data                          | 0.1.6              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Holding                       | 0.1.6              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Bond               | 0.1.7              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Equity             | 0.1.7              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Generic            | 0.1.7              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Swap               | 0.1.7              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Instrument.Token              | 0.1.6              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Account             | 0.1.1              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Claims              | 0.1.6              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Data                | 0.1.5              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Holding             | 0.1.6              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Base     | 0.1.6              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Bond     | 0.1.6              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Equity   | 0.1.7              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Generic  | 0.1.7              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Swap     | 0.1.7              | Alpha  |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Instrument.Token    | 0.1.6              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Lifecycle           | 0.1.7              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Settlement          | 0.1.7              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Types               | 0.1.6              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Interface.Util                | 0.1.5              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Lifecycle                     | 0.1.7              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Settlement                    | 0.1.7              | Beta   |
+--------------------------------------------+--------------------+--------+
| Daml.Finance.Util                          | 0.1.5              | Beta   |
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
