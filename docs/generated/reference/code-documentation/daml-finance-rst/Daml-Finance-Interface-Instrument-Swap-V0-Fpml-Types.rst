.. Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-fpml-types-85330:

Daml.Finance.Interface.Instrument.Swap.V0.Fpml.Types
====================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949:

**data** `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-79074:

  `Fpml <constr-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-79074_>`_

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
         - The description of the swap\.
       * - swapStreams
         - \[:ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>`\]
         - Each element describes a stream of swap payments, for example a regular fixed or floating rate\.
       * - issuerPartyRef
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Used to the identify which counterparty is the issuer in the swapStream\.
       * - calendarDataProvider
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The reference data provider to use for the holiday calendar\.
       * - currencies
         - \[:ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`\]
         - The currencies of the different swap legs, one for each swapStream\. For example, if one leg pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"calendarDataProvider\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currencies\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ \[:ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"fpml\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327>` `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"fpml\" :ref:`View <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-90652>` `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"issuerPartyRef\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lastEventTimestamp\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"swapStreams\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ \[:ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>`\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"calendarDataProvider\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currencies\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ \[:ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"fpml\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327>` `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"fpml\" :ref:`View <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-view-90652>` `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"issuerPartyRef\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lastEventTimestamp\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"swapStreams\" `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_ \[:ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>`\]
