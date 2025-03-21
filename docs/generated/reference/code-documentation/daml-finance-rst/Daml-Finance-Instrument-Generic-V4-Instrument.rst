.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-generic-v4-instrument-39541:

Daml.Finance.Instrument.Generic.V4.Instrument
=============================================

Templates
---------

.. _type-daml-finance-instrument-generic-v4-instrument-instrument-96378:

**template** `Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378_>`_

  An instrument representing a generic payoff, modelled using the Contingent Claims library\.
  The responsibility for processing lifecycle events as well as elections is delegated to the
  issuer, who is hence responsible for providing the correct ``Observable``\\s\.

  Signatory\: depository, issuer

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The instrument depository\.
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The instrument issuer\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - The identifier with corresponding version\.
     * - version
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The instrument's version\.
     * - holdingStandard
       - :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`
       - The holding standard for holdings referencing this instrument\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A human readable description of the instrument\.
     * - claims
       - :ref:`C <type-daml-finance-interface-claims-v4-types-c-76802>`
       - The claim tree\.
     * - acquisitionTime
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - The claim's acquisition time\. This usually corresponds to the start date of the contract\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  + **Choice** Archive

    Controller\: depository, issuer

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-generic-v4-instrument-i-8248>` **for** `Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378_>`_

Data Types
----------

.. _type-daml-finance-instrument-generic-v4-instrument-t-32730:

**type** `T <type-daml-finance-instrument-generic-v4-instrument-t-32730_>`_
  \= `Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378_>`_

  Type synonym for ``Instrument``\.
