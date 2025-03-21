.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-asset-instrument-80683:

Daml.Finance.Interface.Instrument.Swap.V0.Asset.Instrument
==========================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-asset-instrument-instrument-40916:

**interface** `Instrument <type-daml-finance-interface-instrument-swap-v0-asset-instrument-instrument-40916_>`_

  Instrument interface representing an asset swap\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-asset-instrument-v-67426_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-asset-instrument-getview-75677:

    **Choice** `GetView <type-daml-finance-interface-instrument-swap-v0-asset-instrument-getview-75677_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-swap-v0-asset-instrument-v-67426_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.


Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-asset-instrument-i-95573:

**type** `I <type-daml-finance-interface-instrument-swap-v0-asset-instrument-i-95573_>`_
  \= `Instrument <type-daml-finance-interface-instrument-swap-v0-asset-instrument-instrument-40916_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-swap-v0-asset-factory-factory-42186>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-swap-v0-asset-factory-create-72901>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-swap-v0-asset-instrument-i-95573_>`_))

.. _type-daml-finance-interface-instrument-swap-v0-asset-instrument-v-67426:

**type** `V <type-daml-finance-interface-instrument-swap-v0-asset-instrument-v-67426_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-asset-instrument-view-14814_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-swap-v0-asset-instrument-instrument-40916_>`_ `V <type-daml-finance-interface-instrument-swap-v0-asset-instrument-v-67426_>`_

.. _type-daml-finance-interface-instrument-swap-v0-asset-instrument-view-14814:

**data** `View <type-daml-finance-interface-instrument-swap-v0-asset-instrument-view-14814_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-asset-instrument-view-63641:

  `View <constr-daml-finance-interface-instrument-swap-v0-asset-instrument-view-63641_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - asset
         - :ref:`Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409>`
         - Attributes of an asset swap\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-asset-instrument-view-14814_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-asset-instrument-view-14814_>`_
