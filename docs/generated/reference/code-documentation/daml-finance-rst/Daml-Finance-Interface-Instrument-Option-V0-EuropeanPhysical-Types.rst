.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-europeanphysical-types-28777:

Daml.Finance.Interface.Instrument.Option.V0.EuropeanPhysical.Types
==================================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104:

**data** `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_

  Describes the attributes of a physically settled European option\.

  .. _constr-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-36687:

  `European <constr-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-36687_>`_

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
       * - referenceAsset
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The reference asset\. For example, in case of an option on AAPL this should be an AAPL instrument\.
       * - ownerReceives
         - `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - Indicate whether a holding owner of this instrument receives the option payoff\.
       * - optionType
         - :ref:`OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036>`
         - Indicate whether the option is a call or a put\.
       * - strike
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The strike price of the option\.
       * - expiryDate
         - `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The expiry date of the option\.
       * - currency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The currency of the option\. For example, if the option pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - prevEvents
         - \[EventData\]
         - A list of previous elections that have been lifecycled on this instrument so far\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"european\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-create-41404>` `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"european\" :ref:`View <type-daml-finance-interface-instrument-option-v0-europeanphysical-instrument-view-96581>` `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"expiryDate\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lastEventTimestamp\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"optionType\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ :ref:`OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"ownerReceives\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"prevEvents\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ \[EventData\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"referenceAsset\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"strike\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"european\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-create-41404>` `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"european\" :ref:`View <type-daml-finance-interface-instrument-option-v0-europeanphysical-instrument-view-96581>` `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"expiryDate\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lastEventTimestamp\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"optionType\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ :ref:`OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"ownerReceives\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"prevEvents\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ \[EventData\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"referenceAsset\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"strike\" `European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
