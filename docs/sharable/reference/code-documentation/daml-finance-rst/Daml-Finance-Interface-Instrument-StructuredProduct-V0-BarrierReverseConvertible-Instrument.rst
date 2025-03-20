.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-33591:

Daml.Finance.Interface.Instrument.StructuredProduct.V0.BarrierReverseConvertible.Instrument
===========================================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-96792:

**interface** `Instrument <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-96792_>`_

  Instrument interface representing a BRC\.

  **viewtype** `V <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-v-93950_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-getview-82009:

    **Choice** `GetView <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-getview-82009_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-v-93950_>`_

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

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-i-56857:

**type** `I <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-i-56857_>`_
  \= `Instrument <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-96792_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-factory-47150>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-i-56857_>`_))

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-v-93950:

**type** `V <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-v-93950_>`_
  \= `View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-1802_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-96792_>`_ `V <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-v-93950_>`_

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-1802:

**data** `View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-1802_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-26791:

  `View <constr-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-26791_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - barrierReverseConvertible
         - :ref:`BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687>`
         - Attributes of a BRC\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-1802_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-1802_>`_
