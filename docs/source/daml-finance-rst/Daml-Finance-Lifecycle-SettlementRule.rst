.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-settlementrule-58359:

Module Daml.Finance.Lifecycle.SettlementRule
============================================

Templates
---------

.. _type-daml-finance-lifecycle-settlementrule-rule-4603:

**template** `Rule <type-daml-finance-lifecycle-settlementrule-rule-4603_>`_

  Rule contract that allows an actor to claim effects on a given instrument (across all versions)\.
  It returns settlement instructions\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - custodian
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Custodian of the holding upon which an effect can be claimed\.
     * - owner
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Owner of the holding upon which an effect can be claimed\.
     * - claimers
       - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
       - Parties granted the ability to claim an effect\.
     * - instructableCid
       - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-instructable-i-97939>`
       - Instructable contract used to generate the ``Settleable`` and ``Instruction``\\s\.
     * - instrumentLabel
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The textual identifier of the target instrument\.
     * - settler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party triggering settlement of the instructions\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-settlementrule-i-11766>`

Data Types
----------

.. _type-daml-finance-lifecycle-settlementrule-t-18524:

**type** `T <type-daml-finance-lifecycle-settlementrule-t-18524_>`_
  \= `Rule <type-daml-finance-lifecycle-settlementrule-rule-4603_>`_
