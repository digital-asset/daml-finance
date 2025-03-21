.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-36015:

Daml.Finance.Interface.Instrument.StructuredProduct.V0.AutoCallable.Instrument
==============================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-instrument-75792:

**interface** `Instrument <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-instrument-75792_>`_

  Instrument interface representing an AutoCallable\.

  **viewtype** `V <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-v-59478_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-getview-20097:

    **Choice** `GetView <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-getview-20097_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-v-59478_>`_

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

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-i-91905:

**type** `I <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-i-91905_>`_
  \= `Instrument <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-instrument-75792_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-factory-17934>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-i-91905_>`_))

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-v-59478:

**type** `V <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-v-59478_>`_
  \= `View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-56578_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-instrument-75792_>`_ `V <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-v-59478_>`_

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-56578:

**data** `View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-56578_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-86473:

  `View <constr-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-86473_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - autoCallable
         - :ref:`AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435>`
         - Attributes of an AutoCallable\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-56578_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-56578_>`_
