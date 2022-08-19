.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-electioneffect-99924:

Module Daml.Finance.Lifecycle.ElectionEffect
============================================

Templates
---------

.. _type-daml-finance-lifecycle-electioneffect-electioneffect-28146:

**template** `ElectionEffect <type-daml-finance-lifecycle-electioneffect-electioneffect-28146_>`_

  A contract encoding the consequences of an election for a unit of the target instrument\.
  It needs to be claimed with a holding of the right amount and is consumed after claiming\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The effect provider\.
     * - settler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The party settling the effect's consequences\.
     * - custodian
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The custodian of the holding put forward for election\.
     * - owner
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The owner of the holding put forward for election\.
     * - targetInstrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - The target instrument\.
     * - producedInstrument
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - The produced instrument, when it exists\.
     * - amount
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The elected amount\.
     * - consumed
       - \[:ref:`Q <type-daml-finance-interface-asset-instrument-q-31714>`\]
       - Consumed quantities (not including the target instrument)\.
     * - produced
       - \[:ref:`Q <type-daml-finance-interface-asset-instrument-q-31714>`\]
       - Produced quantities (not including the produced instrument)\.
     * - settlementDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The effect's settlement date\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`

Data Types
----------

.. _type-daml-finance-lifecycle-electioneffect-t-5245:

**type** `T <type-daml-finance-lifecycle-electioneffect-t-5245_>`_
  \= `ElectionEffect <type-daml-finance-lifecycle-electioneffect-electioneffect-28146_>`_
