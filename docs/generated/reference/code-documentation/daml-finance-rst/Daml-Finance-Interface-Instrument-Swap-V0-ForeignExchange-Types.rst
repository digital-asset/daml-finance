.. Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-foreignexchange-types-51939:

Daml.Finance.Interface.Instrument.Swap.V0.ForeignExchange.Types
===============================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609:

**data** `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_

  Describes the attributes of a Foreign Exchange swap\.

  .. _constr-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-74586:

  `ForeignExchange <constr-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-74586_>`_

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
       * - firstFxRate
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fx rate used for the first swap payment\.
       * - finalFxRate
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fx rate used for the final swap payment\.
       * - issueDate
         - `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The date when the swap was issued\.
       * - firstPaymentDate
         - `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The first payment date of the swap\.
       * - maturityDate
         - `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The final payment date of the swap\.
       * - baseCurrency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The base currency of the swap, which will be exchanged to another (foreign) currency on the first payment date\. For example, in case of USD this should be a USD cash instrument\.
       * - foreignCurrency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The foreign currency of the swap\. For example, in case of EUR this should be a EUR cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"baseCurrency\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"finalFxRate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"firstFxRate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"firstPaymentDate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignCurrency\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignExchange\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054>` `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignExchange\" :ref:`View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-20015>` `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"issueDate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lastEventTimestamp\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"maturityDate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"baseCurrency\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"finalFxRate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"firstFxRate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"firstPaymentDate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignCurrency\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignExchange\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054>` `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignExchange\" :ref:`View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-view-20015>` `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"issueDate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lastEventTimestamp\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"maturityDate\" `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
