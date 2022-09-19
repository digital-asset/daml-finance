.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml Finance Documentation
##########################

Welcome to the ``Daml Finance`` documentation. Daml Finance is currently an `Early Access Feature <https://docs.daml.com/support/status-definitions.html>`_ in Alpha status.
This page provides an overview of the documentation content as well as suggested starting points. Use the left-hand menu to explore the various sections, or the search bar above for quick navigation.

Content
*******

* :doc:`Overview <overview/intro>`: description of the purpose of the library, its high-level architecture, as well as targeted use cases.
* :doc:`Concepts <concepts/intro>`: explanation of the main concepts used throughout the library, and how they fit together
* :doc:`Packages <packages/intro>`: documentation for each individual package and its contained modules
* :doc:`Tutorials <tutorials/getting-started/intro>`: step-by-step implementation guides across different use cases
* :doc:`Reference <reference/glossary>`: glossary as well as code-level documentation for each package

Starting Points
***************

The following is a suggested learning path to get productive quickly:

#. :doc:`Get started <tutorials/getting-started/intro>` quickly
#. Understand the :doc:`fundamental concepts <concepts/intro>` in depth
#. Learn how to :doc:`model different financial instruments <tutorials/instrument-modeling/intro>`
#. Read up on the :doc:`background, purpose, and intended usage <overview/intro>` of the library
#. Explore the `Daml Finance Reference Application <https://github.com/digital-asset/daml-finance-app>`_

If you are missing content from the documentation, have feedback on the library, or need any help using it, don't hesitate to `open an issue <https://github.com/digital-asset/daml-finance/issues>`_ on the repository.

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
   tutorials/instrument-modeling/intro

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
