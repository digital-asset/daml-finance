.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-35570:

Daml.Finance.Interface.Instrument.Option.V0.BarrierEuropeanCash.Instrument
==========================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-instrument-32577:

**interface** `Instrument <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-instrument-32577_>`_

  Instrument interface representing a barrier option\.

  **viewtype** `V <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-v-24921_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-getview-13574:

    **Choice** `GetView <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-getview-13574_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-v-24921_>`_

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

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-i-62014:

**type** `I <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-i-62014_>`_
  \= `Instrument <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-instrument-32577_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-factory-30599>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-i-62014_>`_))

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-v-24921:

**type** `V <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-v-24921_>`_
  \= `View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38799_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-instrument-32577_>`_ `V <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-v-24921_>`_

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38799:

**data** `View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38799_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38524:

  `View <constr-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38524_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - barrierEuropean
         - :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>`
         - Attributes of a barrier option\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38799_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38799_>`_
