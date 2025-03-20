.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-option-v0-barriereuropeancash-instrument-60573:

Daml.Finance.Instrument.Option.V0.BarrierEuropeanCash.Instrument
================================================================

Templates
---------

.. _type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010:

**template** `Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010_>`_

  This template models a cash settled, automatically excercised barrier option with European
  exercise\.

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
     * - referenceAssetId
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The reference asset ID\. For example, in case of an option on AAPL this should be a valid reference to the AAPL fixings to be used for the payoff calculation\.
     * - ownerReceives
       - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
       - Indicate whether a holding owner of this instrument receives option payoff\.
     * - optionType
       - :ref:`OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036>`
       - Indicate whether the option is a call or a put\.
     * - strike
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The strike price of the option\.
     * - barrier
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The barrier level of the option\.
     * - barrierType
       - :ref:`BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029>`
       - The type of barrier\.
     * - barrierStartDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The start date for barrier observations\.
     * - expiryDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The expiry date of the option\.
     * - currency
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The currency of the option\. For example, if the option pays in USD this should be a USD cash instrument\.
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

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-i-62014>` **for** `Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010_>`_

Data Types
----------

.. _type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-t-93130:

**type** `T <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-t-93130_>`_
  \= `Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010_>`_

  Type synonym for ``Instrument``\.
