.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-interestrate-instrument-99532:

Daml.Finance.Interface.Instrument.Swap.V0.InterestRate.Instrument
=================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-instrument-35823:

**interface** `Instrument <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-instrument-35823_>`_

  Instrument interface representing an interest rate swap\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-v-67467_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-getview-77596:

    **Choice** `GetView <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-getview-77596_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-v-67467_>`_

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

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-i-87180:

**type** `I <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-i-87180_>`_
  \= `Instrument <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-instrument-35823_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-factory-42261>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-create-78116>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-i-87180_>`_))

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-v-67467:

**type** `V <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-v-67467_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-view-1649_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-instrument-35823_>`_ `V <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-v-67467_>`_

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-view-1649:

**data** `View <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-view-1649_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-interestrate-instrument-view-56384:

  `View <constr-daml-finance-interface-instrument-swap-v0-interestrate-instrument-view-56384_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - interestRate
         - :ref:`InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655>`
         - Attributes of an interest rate swap\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-view-1649_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-view-1649_>`_
