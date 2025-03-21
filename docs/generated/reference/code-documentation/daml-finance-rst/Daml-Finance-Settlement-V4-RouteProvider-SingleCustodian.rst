.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-settlement-v4-routeprovider-singlecustodian-88974:

Daml.Finance.Settlement.V4.RouteProvider.SingleCustodian
========================================================

Templates
---------

.. _type-daml-finance-settlement-v4-routeprovider-singlecustodian-singlecustodian-38133:

**template** `SingleCustodian <type-daml-finance-settlement-v4-routeprovider-singlecustodian-singlecustodian-38133_>`_

  Template which implements the ``RouteProvider`` interface\.
  It is used to transform each settlement ``Step`` into a ``RoutedStep`` using a single custodian\.

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
     * - custodian
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The custodian to be used to route each ``Step``\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-settlement-v4-routeprovider-i-81585>` **for** `SingleCustodian <type-daml-finance-settlement-v4-routeprovider-singlecustodian-singlecustodian-38133_>`_
