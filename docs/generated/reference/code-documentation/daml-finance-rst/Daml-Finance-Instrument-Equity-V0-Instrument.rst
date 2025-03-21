.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-equity-v0-instrument-40246:

Daml.Finance.Instrument.Equity.V0.Instrument
============================================

Templates
---------

.. _type-daml-finance-instrument-equity-v0-instrument-instrument-32561:

**template** `Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561_>`_

  An Instrument representing a common stock\.

  Signatory\: depository, issuer

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Issuer\.
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Depository\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - A textual identifier\.
     * - version
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The instrument's version\.
     * - holdingStandard
       - :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`
       - The holding standard for holdings referencing this instrument\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A description of the instrument\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.
     * - validAsOf
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Timestamp as of which the instrument is valid\. This usually coincides with the timestamp of the event that creates the instrument\. It usually does not coincide with ledger time\.

  + **Choice** Archive

    Controller\: depository, issuer

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-equity-v0-instrument-i-47875>` **for** `Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561_>`_

Data Types
----------

.. _type-daml-finance-instrument-equity-v0-instrument-t-80247:

**type** `T <type-daml-finance-instrument-equity-v0-instrument-t-80247_>`_
  \= `Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561_>`_

  Type synonym for ``Instrument``\.
