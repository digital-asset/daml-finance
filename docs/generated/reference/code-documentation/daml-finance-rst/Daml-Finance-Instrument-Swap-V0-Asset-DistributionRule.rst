.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-asset-distributionrule-89667:

Daml.Finance.Instrument.Swap.V0.Asset.DistributionRule
======================================================

Templates
---------

.. _type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789:

**template** `DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789_>`_

  Rule contract used to process a Distribution event (e\.g\. share or cash dividends) targeting an
  underlying of an asset swap\.

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
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Observers of the distribution rule\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Identifier for the rule contract\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Textual description\.
     * - assetSwapFactoryCid
       - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-asset-factory-i-5355>`
       - Factory used to create an updated version of the asset swap\.
     * - newInstrumentObservers
       - \[(`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`)\]
       - Observers used for the updated version of the asset swap\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-i-54386>` **for** `DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789_>`_
