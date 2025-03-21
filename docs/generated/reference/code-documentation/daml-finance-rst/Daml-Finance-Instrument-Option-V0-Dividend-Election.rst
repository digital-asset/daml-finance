.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-option-v0-dividend-election-51824:

Daml.Finance.Instrument.Option.V0.Dividend.Election
===================================================

Templates
---------

.. _type-daml-finance-instrument-option-v0-dividend-election-factory-67569:

**template** `Factory <type-daml-finance-instrument-option-v0-dividend-election-factory-67569_>`_

  Factory template to create an ``Election``\.

  Signatory\: provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The provider of the ``Factory``\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - A set of observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-i-73947>` **for** `Factory <type-daml-finance-instrument-option-v0-dividend-election-factory-67569_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-instrument-option-v0-dividend-election-factory-67569_>`_
