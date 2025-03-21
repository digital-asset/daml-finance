.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-foreignexchange-instrument-58705:

Daml.Finance.Instrument.Swap.V0.ForeignExchange.Instrument
==========================================================

Templates
---------

.. _type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514:

**template** `Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514_>`_

  This template models a foreign exchange swap (FX Swap)\.
  It has two legs\: an initial FX transaction and a final FX transaction\.
  The instrument has a base currency and a foreign currency\.
  The convention is that a holding owner receives the foreign currency in the initial transaction
  (and pays it in the final transaction)\.
  Both FX rates and transaction dates are predetermined between the counterparties\.
  For example\: USD 1000k vs EUR 1100k (fx rate\: 1\.10) today
  USD 1000k vs EUR 1200k (fx rate\: 1\.20) in 6 months

  Signatory\: depository, issuer

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The depository of the instrument\.
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The issuer of the instrument\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - An identifier of the instrument\.
     * - version
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The instrument's version\.
     * - holdingStandard
       - :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`
       - The holding standard for holdings referencing this instrument\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A description of the instrument\.
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
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The observers of the instrument\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  + **Choice** Archive

    Controller\: depository, issuer

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-i-60062>` **for** `Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514_>`_

Data Types
----------

.. _type-daml-finance-instrument-swap-v0-foreignexchange-instrument-t-67930:

**type** `T <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-t-67930_>`_
  \= `Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514_>`_

  Type synonym for ``Instrument``\.
