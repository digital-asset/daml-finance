.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-token-v4-instrument-53415:

Daml.Finance.Instrument.Token.V4.Instrument
===========================================

Templates
---------

.. _type-daml-finance-instrument-token-v4-instrument-instrument-45256:

**template** `Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256_>`_

  Implementation of a Token Instrument, which is a simple instrument whose economic terms
  on the ledger are represented by an ``id`` and a textual ``description``\.

  Signatory\: depository, issuer

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The instrument's depository\.
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The instrument's issuer\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - The intrument's identifier\.
     * - version
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual instrument version\.
     * - holdingStandard
       - :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`
       - The holding standard for holdings referencing this instrument\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A description of the instrument\.
     * - validAsOf
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Timestamp as of which the instrument is valid\. This usually coincides with the timestamp of the event that creates the instrument\. It usually does not coincide with ledger time\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.

  + **Choice** Archive

    Controller\: depository, issuer

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-token-v4-instrument-i-45050>` **for** `Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256_>`_

Data Types
----------

.. _type-daml-finance-instrument-token-v4-instrument-t-54816:

**type** `T <type-daml-finance-instrument-token-v4-instrument-t-54816_>`_
  \= `Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256_>`_

  Type synonym for ``Instrument``\.
