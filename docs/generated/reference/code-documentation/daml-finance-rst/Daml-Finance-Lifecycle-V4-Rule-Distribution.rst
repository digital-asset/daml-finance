.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-rule-distribution-2662:

Daml.Finance.Lifecycle.V4.Rule.Distribution
===========================================

Templates
---------

.. _type-daml-finance-lifecycle-v4-rule-distribution-rule-34:

**template** `Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34_>`_

  Rule contract that defines the distribution of units of an instrument for each unit of a target
  instrument (e\.g\. share or cash dividends)\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Providers of the distribution rule\.
     * - lifecycler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party performing the lifecycling\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Observers of the distribution rule\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Identifier for the rule contract\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Textual description\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-i-54386>` **for** `Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34_>`_

Data Types
----------

.. _type-daml-finance-lifecycle-v4-rule-distribution-t-33867:

**type** `T <type-daml-finance-lifecycle-v4-rule-distribution-t-33867_>`_
  \= `Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34_>`_

  Type synonym for ``Rule``\.
