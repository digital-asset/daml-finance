.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-util-v3-disclosure-50779:

Daml.Finance.Interface.Util.V3.Disclosure
=========================================

Interfaces
----------

.. _type-daml-finance-interface-util-v3-disclosure-disclosure-91672:

**interface** `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_

  An interface for managing the visibility of contracts for non\-authorizing parties\.

  **viewtype** `V <type-daml-finance-interface-util-v3-disclosure-v-30650_>`_

  + .. _type-daml-finance-interface-util-v3-disclosure-addobservers-68807:

    **Choice** `AddObservers <type-daml-finance-interface-util-v3-disclosure-addobservers-68807_>`_

    Add a single new observer context to the existing observers\.

    Controller\: disclosers

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - disclosers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Party calling this choice\.
       * - observersToAdd
         - (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`)
         - Parties to add as observers to the contract and the corresponding observer context\. If the observer context already exists, the new set of parties is added to the old one\.

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-util-v3-disclosure-getview-72965:

    **Choice** `GetView <type-daml-finance-interface-util-v3-disclosure-getview-72965_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-util-v3-disclosure-view-81206_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-util-v3-disclosure-removeobservers-4683:

    **Choice** `RemoveObservers <type-daml-finance-interface-util-v3-disclosure-removeobservers-4683_>`_

    Remove observers from a context\.
    None is returned if no update is needed\. Parties for a context can be removed if any
    of the disclosers are part of the observers to be removed or the disclosureControllers\.

    Controller\: disclosers

    Returns\: `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_)

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - disclosers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties calling this choice\.
       * - observersToRemove
         - (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`)
         - Parties to be removed from the contract observers and the corresponding observer context\.

  + .. _type-daml-finance-interface-util-v3-disclosure-setobservers-68580:

    **Choice** `SetObservers <type-daml-finance-interface-util-v3-disclosure-setobservers-68580_>`_

    Set the observers for a contract\.

    Controller\: disclosers

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - disclosers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Party calling this choice\.
       * - newObservers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - Observers to set for this contract\. This overrides the existing observers\. The parties are mapped by a specific key\. The textual key is the \"observation context\" of the disclosure\. This allows processes to add/remove parties for their specific purpose, without affecting others\.

  + **Method addObservers \:** `AddObservers <type-daml-finance-interface-util-v3-disclosure-addobservers-68807_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_)

    Implementation of the ``AddObservers`` choice\.

  + **Method removeObservers \:** `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_ \-\> `RemoveObservers <type-daml-finance-interface-util-v3-disclosure-removeobservers-4683_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_))

    Implementation of the ``RemoveObservers`` choice\.

  + **Method setObservers \:** `SetObservers <type-daml-finance-interface-util-v3-disclosure-setobservers-68580_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_)

    Implementation of the ``SetObservers`` choice\.

Data Types
----------

.. _type-daml-finance-interface-util-v3-disclosure-i-28317:

**type** `I <type-daml-finance-interface-util-v3-disclosure-i-28317_>`_
  \= `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_

  Type synonym for ``Disclosure``\.

.. _type-daml-finance-interface-util-v3-disclosure-v-30650:

**type** `V <type-daml-finance-interface-util-v3-disclosure-v-30650_>`_
  \= `View <type-daml-finance-interface-util-v3-disclosure-view-81206_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_ `V <type-daml-finance-interface-util-v3-disclosure-v-30650_>`_

.. _type-daml-finance-interface-util-v3-disclosure-view-81206:

**data** `View <type-daml-finance-interface-util-v3-disclosure-view-81206_>`_

  View for ``Disclosure``\.

  .. _constr-daml-finance-interface-util-v3-disclosure-view-5523:

  `View <constr-daml-finance-interface-util-v3-disclosure-view-5523_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - disclosureControllers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Disjunction choice controllers\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - Observers with context\. The parties are mapped by a specific key\. The textual key is the \"observation context\" of the disclosure\. This allows processes to add/remove parties for their specific purpose, without affecting others\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-util-v3-disclosure-view-81206_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-util-v3-disclosure-view-81206_>`_

Functions
---------

.. _function-daml-finance-interface-util-v3-disclosure-setobservers-71616:

`setObservers <function-daml-finance-interface-util-v3-disclosure-setobservers-71616_>`_
  \: `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_ \-\> `SetObservers <type-daml-finance-interface-util-v3-disclosure-setobservers-68580_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_)

.. _function-daml-finance-interface-util-v3-disclosure-addobservers-65651:

`addObservers <function-daml-finance-interface-util-v3-disclosure-addobservers-65651_>`_
  \: `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_ \-\> `AddObservers <type-daml-finance-interface-util-v3-disclosure-addobservers-68807_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_)

.. _function-daml-finance-interface-util-v3-disclosure-removeobservers-95863:

`removeObservers <function-daml-finance-interface-util-v3-disclosure-removeobservers-95863_>`_
  \: `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_ \-\> `RemoveObservers <type-daml-finance-interface-util-v3-disclosure-removeobservers-4683_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-util-v3-disclosure-disclosure-91672_>`_))

.. _function-daml-finance-interface-util-v3-disclosure-flattenobservers-40218:

`flattenObservers <function-daml-finance-interface-util-v3-disclosure-flattenobservers-40218_>`_
  \: :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>` \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  Flattens observers which use the ``PartiesMap`` into a ``Set Party`` for usage in template
  definitions\. For example\:

  .. code-block:: daml

    observer $ flattenObservers observers
