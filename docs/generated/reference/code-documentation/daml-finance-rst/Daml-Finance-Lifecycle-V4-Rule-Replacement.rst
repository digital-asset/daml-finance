.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-rule-replacement-25183:

Daml.Finance.Lifecycle.V4.Rule.Replacement
==========================================

Templates
---------

.. _type-daml-finance-lifecycle-v4-rule-replacement-rule-24043:

**template** `Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043_>`_

  Rule contract that defines the replacement of units of an instrument with a basket of other
  instruments (e\.g\. stock merger)\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Providers of the replacement rule\.
     * - lifecycler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party performing the lifecycling\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Observers\.
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

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-i-54386>` **for** `Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043_>`_

Data Types
----------

.. _type-daml-finance-lifecycle-v4-rule-replacement-t-72652:

**type** `T <type-daml-finance-lifecycle-v4-rule-replacement-t-72652_>`_
  \= `Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043_>`_

  Type synonym for ``Rule``\.
