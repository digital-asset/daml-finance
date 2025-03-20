.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-data-v4-time-ledgertime-80144:

Daml.Finance.Data.V4.Time.LedgerTime
====================================

Templates
---------

.. _type-daml-finance-data-v4-time-ledgertime-ledgertime-59708:

**template** `LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708_>`_

  A ``LedgerTime`` is a template used to retrieve current ledger time as a ``TimeObservable``\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - The time providers\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - The ledger time identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The ledger time description\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Observers\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-i-1376>` **for** `LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708_>`_

Data Types
----------

.. _type-daml-finance-data-v4-time-ledgertime-t-27525:

**type** `T <type-daml-finance-data-v4-time-ledgertime-t-27525_>`_
  \= `LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708_>`_

  Type synonym for ``LedgerTime``\.
