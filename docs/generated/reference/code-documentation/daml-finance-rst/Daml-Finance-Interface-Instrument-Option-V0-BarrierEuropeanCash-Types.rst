.. Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-37351:

Daml.Finance.Interface.Instrument.Option.V0.BarrierEuropeanCash.Types
=====================================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436:

**data** `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_

  Describes the attributes of a cash\-settled barrier option with European exercise\.

  .. _constr-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-65587:

  `BarrierEuropean <constr-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-65587_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument's key\.
       * - description
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The description of the option\.
       * - referenceAssetId
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The reference asset ID\. For example, in case of an option on AAPL this should be a valid reference to the AAPL fixings to be used for the payoff calculation\.
       * - ownerReceives
         - `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - Indicate whether a holding owner of this instrument receives the option payoff\.
       * - optionType
         - :ref:`OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036>`
         - Indicate whether the option is a call or a put\.
       * - strike
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The strike price of the option\.
       * - barrier
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The barrier level of the option\.
       * - barrierType
         - :ref:`BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029>`
         - The type of barrier\.
       * - barrierStartDate
         - `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The start date for barrier observations\.
       * - expiryDate
         - `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The expiry date of the option\.
       * - currency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The currency of the option\. For example, if the option pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrier\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierEuropean\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698>` `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierEuropean\" :ref:`View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38799>` `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierStartDate\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierType\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ :ref:`BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"expiryDate\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lastEventTimestamp\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"optionType\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ :ref:`OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"ownerReceives\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"referenceAssetId\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"strike\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrier\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierEuropean\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698>` `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierEuropean\" :ref:`View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-view-38799>` `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierStartDate\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierType\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ :ref:`BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"expiryDate\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lastEventTimestamp\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"optionType\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ :ref:`OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"ownerReceives\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"referenceAssetId\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"strike\" `BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
