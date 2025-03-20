.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-76870:

Daml.Finance.Interface.Instrument.Swap.V0.ForeignExchange.Instrument
====================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-instrument-7777:

**interface** `Instrument <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-instrument-7777_>`_

  Instrument interface representing an FX swap\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-v-22969_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-getview-42278:

    **Choice** `GetView <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-getview-42278_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-v-22969_>`_

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

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-i-60062:

**type** `I <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-i-60062_>`_
  \= `Instrument <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-instrument-7777_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-factory-72691>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-i-60062_>`_))

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-v-22969:

**type** `V <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-v-22969_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-20015_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-instrument-7777_>`_ `V <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-v-22969_>`_

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-20015:

**data** `View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-20015_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-5620:

  `View <constr-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-5620_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - foreignExchange
         - :ref:`ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609>`
         - Attributes of an FX swap\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-20015_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-20015_>`_
