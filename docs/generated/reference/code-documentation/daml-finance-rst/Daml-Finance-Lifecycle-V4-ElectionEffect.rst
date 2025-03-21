.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-electioneffect-83755:

Daml.Finance.Lifecycle.V4.ElectionEffect
========================================

Templates
---------

.. _type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949:

**template** `ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949_>`_

  A contract encoding the consequences of an election for one unit of the target instrument\.
  It needs to be claimed with the right amount and is consumed after claiming\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - The effect provider\.
     * - custodian
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The custodian of the holding put forward for election\.
     * - owner
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The owner of the holding put forward for election\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - The effect's identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The effect's description\.
     * - targetInstrument
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The target instrument\.
     * - producedInstrument
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The produced instrument, when it exists\.
     * - amount
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The elected amount\.
     * - otherConsumed
       - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
       - Consumed quantities (not including the target instrument)\.
     * - otherProduced
       - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
       - Produced quantities (not including the produced instrument)\.
     * - settlementTime
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - The effect's settlement time (if any)\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>` **for** `ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949_>`_

Data Types
----------

.. _type-daml-finance-lifecycle-v4-electioneffect-t-11392:

**type** `T <type-daml-finance-lifecycle-v4-electioneffect-t-11392_>`_
  \= `ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949_>`_

  Type synonym for ``ElectionEffect``\.
