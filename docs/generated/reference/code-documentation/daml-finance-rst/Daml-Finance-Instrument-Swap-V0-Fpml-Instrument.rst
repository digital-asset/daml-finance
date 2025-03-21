.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-fpml-instrument-27524:

Daml.Finance.Instrument.Swap.V0.Fpml.Instrument
===============================================

Templates
---------

.. _type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235:

**template** `Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235_>`_

  This template models a swap specified by FpML swapStream modules\.
  It can contain one or several legs of different types\: fix or floating rates

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
     * - swapStreams
       - \[:ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>`\]
       - Each element describes a stream of swap payments, for example a regular fixed or floating rate\.
     * - issuerPartyRef
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Used to the identify which counterparty is the issuer in the swapStream\.
     * - calendarDataProvider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The reference data provider to use for the holiday calendar\.
     * - currencies
       - \[:ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`\]
       - The currencies of the different swap legs, one for each swapStream\. For example, if one leg pays in USD this should be a USD cash instrument\.
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

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-i-31607>` **for** `Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235_>`_

Data Types
----------

.. _type-daml-finance-instrument-swap-v0-fpml-instrument-t-92937:

**type** `T <type-daml-finance-instrument-swap-v0-fpml-instrument-t-92937_>`_
  \= `Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235_>`_

  Type synonym for ``Instrument``\.
