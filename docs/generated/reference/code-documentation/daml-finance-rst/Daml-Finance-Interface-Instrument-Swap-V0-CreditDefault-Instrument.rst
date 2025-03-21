.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-94865:

Daml.Finance.Interface.Instrument.Swap.V0.CreditDefault.Instrument
==================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-instrument-93730:

**interface** `Instrument <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-instrument-93730_>`_

  Instrument interface representing a credit default swap\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-v-32980_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-getview-95179:

    **Choice** `GetView <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-getview-95179_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-v-32980_>`_

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

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-i-43747:

**type** `I <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-i-43747_>`_
  \= `Instrument <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-instrument-93730_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-factory-36744>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-create-56287>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-i-43747_>`_))

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-v-32980:

**type** `V <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-v-32980_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-view-28576_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-instrument-93730_>`_ `V <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-v-32980_>`_

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-view-28576:

**data** `View <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-view-28576_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-view-19183:

  `View <constr-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-view-19183_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - creditDefault
         - :ref:`CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509>`
         - Attributes of a credit default swap\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-view-28576_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-view-28576_>`_
