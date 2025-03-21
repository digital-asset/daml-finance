.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-holding-v4-factory-50391:

Daml.Finance.Holding.V4.Factory
===============================

Templates
---------

.. _type-daml-finance-holding-v4-factory-factory-39768:

**template** `Factory <type-daml-finance-holding-v4-factory-factory-39768_>`_

  Implementation of a factory template for holdings\.

  Signatory\: provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The factory's provider\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Identifier for the factory\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The factory's observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-holding-v4-factory-i-40318>` **for** `Factory <type-daml-finance-holding-v4-factory-factory-39768_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-holding-v4-factory-factory-39768_>`_

Data Types
----------

.. _type-daml-finance-holding-v4-factory-t-28124:

**type** `T <type-daml-finance-holding-v4-factory-t-28124_>`_
  \= `Factory <type-daml-finance-holding-v4-factory-factory-39768_>`_

  Type synonym for ``Factory``\.
