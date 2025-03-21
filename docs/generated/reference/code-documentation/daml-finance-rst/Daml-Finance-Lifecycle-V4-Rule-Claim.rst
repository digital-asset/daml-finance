.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-rule-claim-11721:

Daml.Finance.Lifecycle.V4.Rule.Claim
====================================

Templates
---------

.. _type-daml-finance-lifecycle-v4-rule-claim-rule-66621:

**template** `Rule <type-daml-finance-lifecycle-v4-rule-claim-rule-66621_>`_

  Rule contract that allows an actor to claim effects, returning settlement instructions\.

  Signatory\: provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Provider of the claim rule\. Together with the actors of the ``ClaimEffect`` choice the authorization requirements to upgrade the holdings being claimed have to be met\.
     * - claimers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Any of the parties can claim an effect\.
     * - settlers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Any of the parties can trigger settlement of the resulting batch\.
     * - routeProviderCid
       - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-routeprovider-i-81585>`
       - RouteProvider used to discover settlement routes\.
     * - settlementFactoryCid
       - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-factory-i-2953>`
       - Settlement factory contract used to create a ``Batch`` of ``Instruction``\\s\.
     * - netInstructions
       - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
       - Configure whether netting should be enabled for quantities having the same (instrument, sender, receiver)\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-rule-claim-i-38438>` **for** `Rule <type-daml-finance-lifecycle-v4-rule-claim-rule-66621_>`_

Data Types
----------

.. _type-daml-finance-lifecycle-v4-rule-claim-t-44974:

**type** `T <type-daml-finance-lifecycle-v4-rule-claim-t-44974_>`_
  \= `Rule <type-daml-finance-lifecycle-v4-rule-claim-rule-66621_>`_

  Type synonym for ``Rule``\.
