.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
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
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The description of the swap\.
       * - firstFxRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fx rate used for the first swap payment\.
       * - finalFxRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fx rate used for the final swap payment\.
       * - issueDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The date when the swap was issued\.
       * - firstPaymentDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The first payment date of the swap\.
       * - maturityDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The final payment date of the swap\.
       * - baseCurrency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The base currency of the swap, which will be exchanged to another (foreign) currency on the first payment date\. For example, in case of USD this should be a USD cash instrument\.
       * - foreignCurrency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The foreign currency of the swap\. For example, in case of EUR this should be a EUR cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609_>`_
