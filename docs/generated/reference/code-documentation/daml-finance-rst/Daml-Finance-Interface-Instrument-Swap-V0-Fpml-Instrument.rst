.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-fpml-instrument-92389:

Daml.Finance.Interface.Instrument.Swap.V0.Fpml.Instrument
=========================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-fpml-instrument-instrument-32406:

**interface** `Instrument <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-instrument-32406_>`_

  Instrument interface representing a swap specified as FpML swapStreams\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-v-25120_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-fpml-instrument-getview-66823:

    **Choice** `GetView <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-getview-66823_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-v-25120_>`_

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

.. _type-daml-finance-interface-instrument-swap-v0-fpml-instrument-i-31607:

**type** `I <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-i-31607_>`_
  \= `Instrument <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-instrument-32406_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-swap-v0-fpml-factory-factory-93528>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-i-31607_>`_))

.. _type-daml-finance-interface-instrument-swap-v0-fpml-instrument-v-25120:

**type** `V <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-v-25120_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-90652_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-instrument-32406_>`_ `V <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-v-25120_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-90652:

**data** `View <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-90652_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-16065:

  `View <constr-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-16065_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - fpml
         - :ref:`Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949>`
         - Attributes of a swap specified as FpML swapStreams\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-90652_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-90652_>`_
