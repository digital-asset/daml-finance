.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-equity-v0-instrument-90217:

Daml.Finance.Interface.Instrument.Equity.V0.Instrument
======================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-equity-v0-instrument-instrument-31490:

**interface** `Instrument <type-daml-finance-interface-instrument-equity-v0-instrument-instrument-31490_>`_

  An interface for a generic equity instrument\.

  **viewtype** `V <type-daml-finance-interface-instrument-equity-v0-instrument-v-37108_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612:

    **Choice** `DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612_>`_

    Declare a distribution (e\.g\. a dividend or a rights issue) to shareholders\.

    Controller\: (DA\.Internal\.Record\.getField @\"issuer\" (view $ toInterface @BaseInstrument\.I this))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Event identifier of the dividend distribution\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Description of the dividend event\.
       * - effectiveTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Time at which the dividend is distributed\.
       * - newInstrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - Instrument held after the dividend distribution (i\.e\. \"ex\-dividend\" stock)\.
       * - perUnitDistribution
         - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
         - Distributed quantities per unit held\.

  + .. _type-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-46147:

    **Choice** `DeclareReplacement <type-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-46147_>`_

    Declare a replacement event, where units of the instrument are replaced by a basket of
    other instruments\.

    Controller\: (DA\.Internal\.Record\.getField @\"issuer\" (view $ toInterface @BaseInstrument\.I this))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Distribution Id\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Description of the replacement event\.
       * - effectiveTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Time the replacement is to be executed\.
       * - perUnitReplacement
         - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
         - Payout offered to shareholders per held share\.

  + .. _type-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-89514:

    **Choice** `DeclareStockSplit <type-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-89514_>`_

    Declare a stock split\.

    Controller\: (DA\.Internal\.Record\.getField @\"issuer\" (view $ toInterface @BaseInstrument\.I this))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Event identifier of the stock split\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Description of the stock split event\.
       * - effectiveTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Time at which the stock split is effective\.
       * - newInstrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - Instrument to be held after the stock split is executed\.
       * - adjustmentFactor
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Adjustment factor for the stock split\.

  + .. _type-daml-finance-interface-instrument-equity-v0-instrument-getview-66507:

    **Choice** `GetView <type-daml-finance-interface-instrument-equity-v0-instrument-getview-66507_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-instrument-equity-v0-instrument-view-97536_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method declareDistribution \:** `DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`)

    Implementation for the ``DeclareDistribution`` choice\.

  + **Method declareReplacement \:** `DeclareReplacement <type-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-46147_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`)

    Implementation for the ``DeclareReplacement`` choice\.

  + **Method declareStockSplit \:** `DeclareStockSplit <type-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-89514_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`)

    Implementation for the ``DeclareStockSplit`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-equity-v0-instrument-i-47875:

**type** `I <type-daml-finance-interface-instrument-equity-v0-instrument-i-47875_>`_
  \= `Instrument <type-daml-finance-interface-instrument-equity-v0-instrument-instrument-31490_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-equity-v0-factory-factory-21456>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-equity-v0-factory-create-45111>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-equity-v0-instrument-i-47875_>`_))

.. _type-daml-finance-interface-instrument-equity-v0-instrument-v-37108:

**type** `V <type-daml-finance-interface-instrument-equity-v0-instrument-v-37108_>`_
  \= `View <type-daml-finance-interface-instrument-equity-v0-instrument-view-97536_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-equity-v0-instrument-instrument-31490_>`_ `V <type-daml-finance-interface-instrument-equity-v0-instrument-v-37108_>`_

.. _type-daml-finance-interface-instrument-equity-v0-instrument-view-97536:

**data** `View <type-daml-finance-interface-instrument-equity-v0-instrument-view-97536_>`_

  View for ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-equity-v0-instrument-view-27859:

  `View <constr-daml-finance-interface-instrument-equity-v0-instrument-view-27859_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument's key\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-equity-v0-instrument-view-97536_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-equity-v0-instrument-view-97536_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-82168:

`declareDistribution <function-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-82168_>`_
  \: `Instrument <type-daml-finance-interface-instrument-equity-v0-instrument-instrument-31490_>`_ \-\> `DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`)

.. _function-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-44310:

`declareStockSplit <function-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-44310_>`_
  \: `Instrument <type-daml-finance-interface-instrument-equity-v0-instrument-instrument-31490_>`_ \-\> `DeclareStockSplit <type-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-89514_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`)

.. _function-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-18879:

`declareReplacement <function-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-18879_>`_
  \: `Instrument <type-daml-finance-interface-instrument-equity-v0-instrument-instrument-31490_>`_ \-\> `DeclareReplacement <type-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-46147_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`)
