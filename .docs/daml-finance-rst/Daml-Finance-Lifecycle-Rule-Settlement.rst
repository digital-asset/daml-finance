.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-rule-settlement-14148:

Module Daml.Finance.Lifecycle.Rule.Settlement
=============================================

Templates
---------

.. _type-daml-finance-lifecycle-rule-settlement-rule-56132:

**template** `Rule <type-daml-finance-lifecycle-rule-settlement-rule-56132_>`_

  Rule contract that allows an actor to claim effects, returning settlement instructions\.
  
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
     * - settler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party triggering settlement of the instructions\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-settlementrule-i-11766>`

Data Types
----------

.. _type-daml-finance-lifecycle-rule-settlement-t-38381:

**type** `T <type-daml-finance-lifecycle-rule-settlement-t-38381_>`_
  \= `Rule <type-daml-finance-lifecycle-rule-settlement-rule-56132_>`_
