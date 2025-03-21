.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-bond-v3-fixedrate-factory-55391:

Daml.Finance.Instrument.Bond.V3.FixedRate.Factory
=================================================

Templates
---------

.. _type-daml-finance-instrument-bond-v3-fixedrate-factory-factory-38968:

**template** `Factory <type-daml-finance-instrument-bond-v3-fixedrate-factory-factory-38968_>`_

  Factory template for instrument creation\.

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
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The factory's observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-i-96150>` **for** `Factory <type-daml-finance-instrument-bond-v3-fixedrate-factory-factory-38968_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-instrument-bond-v3-fixedrate-factory-factory-38968_>`_

Data Types
----------

.. _type-daml-finance-instrument-bond-v3-fixedrate-factory-t-21596:

**type** `T <type-daml-finance-instrument-bond-v3-fixedrate-factory-t-21596_>`_
  \= `Factory <type-daml-finance-instrument-bond-v3-fixedrate-factory-factory-38968_>`_

  Type synonym for ``Factory``\.
