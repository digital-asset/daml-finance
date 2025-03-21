.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Architecture
############

This page outlines the architecture of the library and the relationships between the different
packages.

Daml Finance consists of a set of ``.dar`` packages that can be divided into two layers:

- an *interface layer* representing its public, stable API
- an *implementation layer* providing a set of default implementation packages

.. _interface-layer:

Interface Layer
***************

The interface layer provides common types and Daml interface definitions that represent the public
API of Daml Finance. It includes several Daml packages, each grouping related business functions.
These packages can in principle be used independently of each other.

The interface layer consists of the following packages:

- ``Daml.Finance.Interface.Holding.V4`` defines interfaces for holdings and related properties such
  as :ref:`transferability <transferability>` or :ref:`fungibility <fungibility>`.
- ``Daml.Finance.Interface.Account.V4`` defines interfaces for accounts
- ``Daml.Finance.Interface.Settlement.V4`` defines interfaces for settlement route providers,
  settlement instructions, and batched settlements
- ``Daml.Finance.Interface.Lifecycle.V4`` defines interfaces used for instrument lifecycling
- ``Daml.Finance.Interface.Instrument.*`` contains interfaces used for different instrument types
- ``Daml.Finance.Interface.Claims.V4`` contains interfaces used for
  :doc:`Contingent Claims <../instruments/generic/contingent-claims>` based instrument types
- ``Daml.Finance.Interface.Data.V4`` defines interfaces related to reference data
- ``Daml.Finance.Interface.Types.Common.V3`` provides common types
- ``Daml.Finance.Interface.Types.Date.V3`` provides types related to dates
- ``Daml.Finance.Interface.Util.V3`` defines utilities and interfaces used by other interface
  packages.
- ``ContingentClaims.Core.V3`` contains types for representing
  :doc:`Contingent Claims <../instruments/generic/contingent-claims>` tree structures.

Implementation Layer
********************

The implementation layer contains concrete template definitions implementing the interfaces defined
in the interface layer. These represent the contracts that are ultimately stored on the ledger.

For instance, ``Daml.Finance.Holding.V4`` contains a concrete implementation of a
:ref:`Transferable <module-daml-finance-interface-holding-v4-transferable-93054>` and
:ref:`Fungible <module-daml-finance-interface-holding-v4-fungible-55495>` holding. These
interfaces are defined in ``Daml.Finance.Interface.Holding.V4``.

The implementation layer consists of the following packages:

- ``Daml.Finance.Holding.V4`` defines default implementations for holdings
- ``Daml.Finance.Account.V4`` defines default implementations for accounts
- ``Daml.Finance.Settlement.V4`` defines templates for settlement route providers, settlement
  instructions, and batched settlements
- ``Daml.Finance.Lifecycle.V4`` defines an implementation of lifecycle effects and a rule template to
  facilitate their settlement
- ``Daml.Finance.Instrument.*`` contains implementations for various instrument types
- ``Daml.Finance.Data.V4`` includes templates used to store reference data on the ledger
- ``Daml.Finance.Claims.V3`` contains utility functions relating to
  :doc:`Contingent Claims <../instruments/generic/contingent-claims>` based instruments and
  lifecycling
- ``Daml.Finance.Util.V4`` provides a set of pure utility functions mainly for date manipulation
- ``ContingentClaims.Lifecycle.V3`` provides lifecycle utility functions for
  :doc:`Contingent Claims <../instruments/generic/contingent-claims>` based instruments
- ``ContingentClaims.Valuation.V0`` contains experimental functions to transform
  :doc:`Contingent Claims <../instruments/generic/contingent-claims>` instrument trees into a
  mathematical representation suitable for integration with pricing and risk frameworks

Versioning and Compatibility
****************************

Daml Finance follows the semantic versioning scheme.

The interface packages define the public API of the library. Specifically, the interface definitions
which include interface views, methods and choices are guaranteed to remain stable within a major
version of a package. Note that this does not include the package id itself. So purely additive
(e.g. adding new interfaces), or non-functional changes (like compiling a package with a later SDK
version), which do change the package id of a package but do not change the interface definitions,
can be released in minor or patch version increments. Such changes will require dependent
applications to be recompiled and upgraded, but the upgrades are trivial as none of the existing
interfaces changed functionally.

Implementation packages follow a similar convention. A purely additive change, or a change that
does not affect the implemented interfaces can be rolled out as a minor or patch version increase.
Similarly, an upgrade to implement a new *minor or patch* version of of an interface, which doesn't
functionally change the interface implementation is also considered a minor or patch version
increase of an implementation package. If an implementation package changes to implement a new major
version of an interface the major version of the implementation will change as well.

We intend to document the upgrade process and/or provide sample upgrade scripts for contracts within
the Daml Finance perimeter for major version upgrades only.

Note that deprecations of package versions only happen in the context of a Daml SDK release. They
will be listed in the :ref:`release section <releases>` of the documentation and follow the standard
Daml component
`deprecation guidelines <https://docs.daml.com/support/status-definitions.html#deprecation>`_.
