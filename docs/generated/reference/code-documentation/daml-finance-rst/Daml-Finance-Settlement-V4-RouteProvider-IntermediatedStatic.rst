.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-settlement-v4-routeprovider-intermediatedstatic-92315:

Daml.Finance.Settlement.V4.RouteProvider.IntermediatedStatic
============================================================

Templates
---------

.. _type-daml-finance-settlement-v4-routeprovider-intermediatedstatic-intermediatedstatic-19069:

**template** `IntermediatedStatic <type-daml-finance-settlement-v4-routeprovider-intermediatedstatic-intermediatedstatic-19069_>`_

  Template which implements the ``RouteProvider`` interface\.
  It is used to discover the settlement route for each settlement ``Step``, i\.e\., ``RoutedSteps``\\s\.
  For each instrument to settle as part of the batch, a hierarchy of intermediaries is specified
  in ``paths``\. This hierarchy is used to generate the ``RoutedStep``\\s\.

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
     * - paths
       - `Map <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ :ref:`Hierarchy <type-daml-finance-settlement-v4-hierarchy-hierarchy-41337>`
       - Hierarchical paths used to settle holding transfers\. A path is specified for each instrument label\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-settlement-v4-routeprovider-i-81585>` **for** `IntermediatedStatic <type-daml-finance-settlement-v4-routeprovider-intermediatedstatic-intermediatedstatic-19069_>`_
