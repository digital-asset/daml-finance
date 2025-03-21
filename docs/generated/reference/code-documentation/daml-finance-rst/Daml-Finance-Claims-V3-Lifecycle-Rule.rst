.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-claims-v3-lifecycle-rule-196:

Daml.Finance.Claims.V3.Lifecycle.Rule
=====================================

Templates
---------

.. _type-daml-finance-claims-v3-lifecycle-rule-rule-14024:

**template** `Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024_>`_

  Rule to process an event for instruments that are modelled using \"on\-the\-fly\" claims
  (the tree is not stored on\-ledger but generated dynamically)\.
  This rule supports both time update events and election events\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Providers of the lifecycling rule\.
     * - lifecycler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party performing the lifecycling\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers of the rule\.
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

  + **interface instance** :ref:`Exercisable <type-daml-finance-interface-lifecycle-v4-election-exercisable-36259>` **for** `Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-i-54386>` **for** `Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024_>`_
