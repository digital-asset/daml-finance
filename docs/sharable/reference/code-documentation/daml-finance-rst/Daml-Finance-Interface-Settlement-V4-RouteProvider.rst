.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-settlement-v4-routeprovider-63:

Daml.Finance.Interface.Settlement.V4.RouteProvider
==================================================

Interfaces
----------

.. _type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628:

**interface** `RouteProvider <type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628_>`_

  An interface used to discover the settlement route for each ``Step``, i\.e\., ``[RoutedStep]``\.

  **viewtype** `V <type-daml-finance-interface-settlement-v4-routeprovider-v-88198_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-settlement-v4-routeprovider-discover-692:

    **Choice** `Discover <type-daml-finance-interface-settlement-v4-routeprovider-discover-692_>`_

    Discover the settlement route for each ``Step``\.

    Controller\: discoverors

    Returns\: \[:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`\]

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - discoverors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties requesting to discover\.
       * - contextId
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Context for the discovery\.
       * - steps
         - \[:ref:`Step <type-daml-finance-interface-settlement-v4-types-step-16302>`\]
         - Settlement steps to route\.

  + .. _type-daml-finance-interface-settlement-v4-routeprovider-getview-24625:

    **Choice** `GetView <type-daml-finance-interface-settlement-v4-routeprovider-getview-24625_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-settlement-v4-routeprovider-view-58066_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method discover \:** `Discover <type-daml-finance-interface-settlement-v4-routeprovider-discover-692_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`\]

    Implementation of the ``Discover`` choice\.

Data Types
----------

.. _type-daml-finance-interface-settlement-v4-routeprovider-i-81585:

**type** `I <type-daml-finance-interface-settlement-v4-routeprovider-i-81585_>`_
  \= `RouteProvider <type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628_>`_

  Type synonym for ``RouteProvider``\.

.. _type-daml-finance-interface-settlement-v4-routeprovider-v-88198:

**type** `V <type-daml-finance-interface-settlement-v4-routeprovider-v-88198_>`_
  \= `View <type-daml-finance-interface-settlement-v4-routeprovider-view-58066_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `RouteProvider <type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628_>`_ `V <type-daml-finance-interface-settlement-v4-routeprovider-v-88198_>`_

.. _type-daml-finance-interface-settlement-v4-routeprovider-view-58066:

**data** `View <type-daml-finance-interface-settlement-v4-routeprovider-view-58066_>`_

  View for ``RouteProvider``\.

  .. _constr-daml-finance-interface-settlement-v4-routeprovider-view-84673:

  `View <constr-daml-finance-interface-settlement-v4-routeprovider-view-84673_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing the ``RouteProvider`` facility\.
       * - observers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Observers\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-settlement-v4-routeprovider-view-58066_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-settlement-v4-routeprovider-view-58066_>`_

Functions
---------

.. _function-daml-finance-interface-settlement-v4-routeprovider-discover-78064:

`discover <function-daml-finance-interface-settlement-v4-routeprovider-discover-78064_>`_
  \: `RouteProvider <type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628_>`_ \-\> `Discover <type-daml-finance-interface-settlement-v4-routeprovider-discover-692_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`\]
