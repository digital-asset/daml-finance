.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-effect-1975:

Module Daml.Finance.Lifecycle.Effect
====================================

Templates
---------

.. _type-daml-finance-lifecycle-effect-effect-18432:

**template** `Effect <type-daml-finance-lifecycle-effect-effect-18432_>`_

  A contract encoding the consequences of a lifecycle event for a unit of the target instrument\.

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
     * - targetInstrument
       - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-75164>`
       - The target instrument\.
     * - producedInstrument
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-75164>`
       - The produced instrument, when it exists\.
     * - consumed
       - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-31714>`\]
       - Consumed quantities (not including the target instrument)\.
     * - produced
       - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-31714>`\]
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

.. _type-daml-finance-lifecycle-effect-t-84092:

**type** `T <type-daml-finance-lifecycle-effect-t-84092_>`_
  \= `Effect <type-daml-finance-lifecycle-effect-effect-18432_>`_

  **instance** :ref:`HasImplementation <class-daml-finance-interface-lifecycle-effect-hasimplementation-26488>` `T <type-daml-finance-lifecycle-effect-t-84092_>`_
