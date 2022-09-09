.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml Finance Documentation
##########################

Welcome to the ``Daml Finance`` documentation. Daml Finance is currently an `Early Access Feature <https://docs.daml.com/support/status-definitions.html>`_ in Alpha status.
This page provides an overview of the documentation content as well as suggested starting points. Use the left-hand menu to explore the various sections, or the search bar above for quick navigation.

Content
*******

* `Overview <overview/intro>`: description of the purpose of the library, its high-level architecture, as well as targeted use cases.
* `Concepts <concepts/intro>`: explanation of the main concepts used throughout the library, and how they fit together
* `Packages <packages/intro>`: documentation for each individual package and its contained modules
* `Tutorials <tutorials/intro>`: step-by-step implementation guides across different use cases
* `Reference <reference/intro>`: glossary as well as code-level documentation for each package

Starting Points
***************

The following is a suggested learning path to get productive quickly:

#. Get started quickly: `<tutorials/getting-started/intro>`
#. Understand the fundamental concepts in depth: `<overview/core-concepts>`
#. Learn how to model and implement different financial instruments: `<tutorials/instrument-modeling/intro>``
#. Read up on background, purpose, and intended usage of the library: `<overview/intro>``
#. Explore advanced topics following the tutorials: `<tutorials>`

If you are missing content from the documentation, have feedback on the library, or need any help using it, don't hesitate to <open an issue> on the repository.

.. toctree::
   :maxdepth: 0
   :hidden:

   self

.. toctree::
   :titlesonly:
   :hidden:
   :caption: Overview

   overview/intro
   overview/architecture

.. toctree::
   :titlesonly:
   :hidden:
   :caption: Concepts

   concepts/intro
   concepts/asset-model
   concepts/settlement
   concepts/lifecycling

.. toctree::
   :titlesonly:
   :hidden:
   :caption: Packages

   packages/intro
   packages/core-interfaces
   packages/core-implementations
   packages/instrument-extensions

.. toctree::
   :titlesonly:
   :hidden:
   :caption: Tutorials

   tutorials/getting-started/intro
   tutorials/instrument-modelling/intro

.. toctree::
   :titlesonly:
   :hidden:
   :caption: Reference

   reference/glossary
   reference/code-documentation/daml-finance-rst/index

.. Indices and tables
.. ==================

.. * :ref:`genindex`
.. * :ref:`modindex`
.. * :ref:`search`
