.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-generic-v4-lifecycle-rule-7812:

Daml.Finance.Instrument.Generic.V4.Lifecycle.Rule
=================================================

Templates
---------

.. _type-daml-finance-instrument-generic-v4-lifecycle-rule-rule-21784:

**template** `Rule <type-daml-finance-instrument-generic-v4-lifecycle-rule-rule-21784_>`_

  Rule to process a lifecycle event\.
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
       - Providers of the distribution rule\.
     * - lifecycler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party performing the lifecycling\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
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
