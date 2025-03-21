.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-option-v0-dividend-instrument-16416:

Daml.Finance.Instrument.Option.V0.Dividend.Instrument
=====================================================

Templates
---------

.. _type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507:

**template** `Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507_>`_

  This template models a physically settled Dividend option\.
  The holder gets to choose to receive the dividend in cash or in a different form (in shares
  and/or in a foreign currency)\.

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
     * - expiryDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The expiry date of the option\.
     * - cashQuantity
       - :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
       - Dividend paid in cash
     * - sharesQuantity
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
       - Dividend paid in shares (if applicable)
     * - fxQuantity
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
       - Dividend paid in a foreign currency (if applicable)
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The observers of the instrument\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
     * - prevEvents
       - \[EventData\]
       - A list of previous elections that have been lifecycled on this instrument so far\.

  + **Choice** Archive

    Controller\: depository, issuer

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-option-v0-dividend-instrument-i-22979>` **for** `Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507_>`_

Data Types
----------

.. _type-daml-finance-instrument-option-v0-dividend-instrument-t-35369:

**type** `T <type-daml-finance-instrument-option-v0-dividend-instrument-t-35369_>`_
  \= `Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507_>`_

  Type synonym for ``Instrument``\.
