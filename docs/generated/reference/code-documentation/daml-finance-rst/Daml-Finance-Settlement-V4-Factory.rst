.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-settlement-v4-factory-65040:

Daml.Finance.Settlement.V4.Factory
==================================

Templates
---------

.. _type-daml-finance-settlement-v4-factory-factory-91685:

**template** `Factory <type-daml-finance-settlement-v4-factory-factory-91685_>`_

  Factory template that implements the ``Factory`` interface\.
  It is used to create a set of settlement ``Instruction``\\s, and a ``Batch`` to atomically settle
  them\.

  Signatory\: provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party providing the facility\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-settlement-v4-factory-i-2953>` **for** `Factory <type-daml-finance-settlement-v4-factory-factory-91685_>`_
