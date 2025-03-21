.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-rule-claim-89954:

Daml.Finance.Interface.Lifecycle.V4.Rule.Claim
==============================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-rule-claim-claim-387:

**interface** `Claim <type-daml-finance-interface-lifecycle-v4-rule-claim-claim-387_>`_

  Interface for contracts that allow holders to claim an ``Effect`` and generate
  ``SettlementInstruction``\\s\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-rule-claim-v-31825_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-78754:

    **Choice** `ClaimEffect <type-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-78754_>`_

    Claim an effect and generate corresponding settlement instructions\.

    Controller\: claimer

    Returns\: `ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - claimer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party claiming the effect\.
       * - holdingCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`\]
         - The holdings to process\.
       * - effectCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`
         - The effect to process\.
       * - batchId
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Identifier used for the generated settlement batch\.

  + .. _type-daml-finance-interface-lifecycle-v4-rule-claim-getview-7150:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-rule-claim-getview-7150_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method claimEffect \:** `ClaimEffect <type-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-78754_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226_>`_

    Implementation of the ``ClaimEffect`` choice\.

Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226:

**data** `ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226_>`_

  Data type wrapping the results of ``Claim``ing an ``Effect``\.

  .. _constr-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-83491:

  `ClaimResult <constr-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-83491_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - batchCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-batch-i-86753>`
         - Batch used to batch\-settle settlement instructions\.
       * - instructionCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-instruction-i-65587>`\]
         - Settlement instructions to settle all effect consequences\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226_>`_

  **instance** HasMethod `Claim <type-daml-finance-interface-lifecycle-v4-rule-claim-claim-387_>`_ \"claimEffect\" (`ClaimEffect <type-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-78754_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226_>`_)

.. _type-daml-finance-interface-lifecycle-v4-rule-claim-i-38438:

**type** `I <type-daml-finance-interface-lifecycle-v4-rule-claim-i-38438_>`_
  \= `Claim <type-daml-finance-interface-lifecycle-v4-rule-claim-claim-387_>`_

  Type synonym for ``Claim``\.

.. _type-daml-finance-interface-lifecycle-v4-rule-claim-v-31825:

**type** `V <type-daml-finance-interface-lifecycle-v4-rule-claim-v-31825_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Claim <type-daml-finance-interface-lifecycle-v4-rule-claim-claim-387_>`_ `V <type-daml-finance-interface-lifecycle-v4-rule-claim-v-31825_>`_

.. _type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471:

**data** `View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471_>`_

  View for ``Settlement``\.

  .. _constr-daml-finance-interface-lifecycle-v4-rule-claim-view-60072:

  `View <constr-daml-finance-interface-lifecycle-v4-rule-claim-view-60072_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - providers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Providers of the claim rule\. Together with the actors of the ``ClaimEffect`` choice the authorization requirements to upgrade the holdings being claimed have to be met\.
       * - claimers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Any of the parties can claim an effect\.
       * - settlers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Any of the parties can trigger settlement of the resulting batch\.
       * - routeProviderCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-routeprovider-i-81585>`
         - RouteProvider contract used to discover settlement routes\.
       * - settlementFactoryCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-factory-i-2953>`
         - Settlement factory contract used to create a ``Batch`` of ``Instruction``\\s\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-64470:

`claimEffect <function-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-64470_>`_
  \: `Claim <type-daml-finance-interface-lifecycle-v4-rule-claim-claim-387_>`_ \-\> `ClaimEffect <type-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-78754_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226_>`_
