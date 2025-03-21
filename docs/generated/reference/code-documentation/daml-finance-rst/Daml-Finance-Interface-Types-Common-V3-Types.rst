.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-types-common-v3-types-97540:

Daml.Finance.Interface.Types.Common.V3.Types
============================================

Data Types
----------

.. _type-daml-finance-interface-types-common-v3-types-accountkey-55962:

**data** `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  A unique key for Accounts\.

  .. _constr-daml-finance-interface-types-common-v3-types-accountkey-81729:

  `AccountKey <constr-daml-finance-interface-types-common-v3-types-accountkey-81729_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - custodian
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Account provider\.
       * - owner
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Account owner\.
       * - id
         - `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_
         - Unique identifier for an account\.

  **instance** HasInterfaceKey :ref:`Account <type-daml-finance-interface-account-v4-account-account-93407>` :ref:`View <type-daml-finance-interface-account-v4-account-view-18066>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ Reference GetCid SetCid SetObservers :ref:`GetView <type-daml-finance-interface-account-v4-account-getview-21073>`

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** HasMethod :ref:`Account <type-daml-finance-interface-account-v4-account-account-93407>` \"getKey\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ GetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Account <type-daml-finance-interface-account-v4-account-account-93407>`)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ SetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ SetObservers (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

.. _type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007:

**data** `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  A unique key for a holding factory\.

  .. _constr-daml-finance-interface-types-common-v3-types-holdingfactorykey-57870:

  `HoldingFactoryKey <constr-daml-finance-interface-types-common-v3-types-holdingfactorykey-57870_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Holding factory provider\.
       * - id
         - `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_
         - Unique identifier for a holding factory\.

  **instance** HasInterfaceKey :ref:`Factory <type-daml-finance-interface-holding-v4-factory-factory-22859>` :ref:`View <type-daml-finance-interface-holding-v4-factory-view-66511>` `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ Reference GetCid SetCid SetObservers :ref:`GetView <type-daml-finance-interface-holding-v4-factory-getview-97414>`

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-holding-v4-factory-factory-22859>` \"getKey\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ GetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Factory <type-daml-finance-interface-holding-v4-factory-factory-22859>`)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ SetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ SetObservers (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

.. _type-daml-finance-interface-types-common-v3-types-holdingstandard-63293:

**data** `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  The ``HoldingStandard`` data type categorizes holdings into four distinct classes, each defined
  by the combination of holding interfaces they implement\: ``Transferable.I``, ``Fungible.I``, and
  ``Holding.I``\. Notably, ``Transferable.I`` and ``Fungible.I`` both require the implementation of the
  ``Holding.I`` interface\. Please also note that, in this context, \"Transferable\.I\" refers to the
  capability of a holding to be transferred between parties (e\.g\., from Alice to Bob) through a
  single custodian\. Additionally, it is important to highlight that all classes of holdings can be
  credited and debited, and thereby effectively settled\.

  .. _constr-daml-finance-interface-types-common-v3-types-transferablefungible-71114:

  `TransferableFungible <constr-daml-finance-interface-types-common-v3-types-transferablefungible-71114_>`_

    Represents the class of holdings which implement ``Fungible.I`` and ``Transferable.I``, and by
    extension ``Holding.I``\. Holdings in this class are both interchangeable (fungible) and
    can be transferred between parties (such as Alice to Bob) via a custodian\.

  .. _constr-daml-finance-interface-types-common-v3-types-transferable-76192:

  `Transferable <constr-daml-finance-interface-types-common-v3-types-transferable-76192_>`_

    Represents the class of holdings which implement ``Transferable.I`` and, by extension,
    ``Holding.I``, but not ``Fungible.I``\. This class pertains to assets that can be transferred
    between parties through a custodian, but are not interchangeable\.

  .. _constr-daml-finance-interface-types-common-v3-types-fungible-50755:

  `Fungible <constr-daml-finance-interface-types-common-v3-types-fungible-50755_>`_

    Represents the class of holdings which implement ``Fungible.I`` and, by extension,
    ``Holding.I``, but not ``Transferable.I``\. These holdings are interchangeable, suitable for
    scenarios where asset fungibility is crucial, but do not have the transfer capability
    between parties via a custodian\.

  .. _constr-daml-finance-interface-types-common-v3-types-baseholding-71474:

  `BaseHolding <constr-daml-finance-interface-types-common-v3-types-baseholding-71474_>`_

    Represents the class of holdings which implement only ``Holding.I`` and neither
    ``Transferable.I`` nor ``Fungible.I``\. This class encompasses basic holdings without the
    functionalities of custodian\-based transferability or fungibility\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

.. _type-daml-finance-interface-types-common-v3-types-id-28519:

**data** `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  .. _constr-daml-finance-interface-types-common-v3-types-id-84864:

  `Id <constr-daml-finance-interface-types-common-v3-types-id-84864_>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_


  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

.. _type-daml-finance-interface-types-common-v3-types-instrumentkey-82717:

**data** `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  A unique key for Instruments\.

  .. _constr-daml-finance-interface-types-common-v3-types-instrumentkey-49116:

  `InstrumentKey <constr-daml-finance-interface-types-common-v3-types-instrumentkey-49116_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - depository
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing depository services\.
       * - issuer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Issuer of instrument\.
       * - id
         - `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_
         - A unique identifier for an instrument\.
       * - version
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual instrument version\.
       * - holdingStandard
         - `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_
         - The used holding standard for the instrument\.

  **instance** HasInterfaceKey :ref:`Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494>` :ref:`View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ Reference GetCid SetCid SetObservers :ref:`GetView <type-daml-finance-interface-instrument-base-v4-instrument-getview-66559>`

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494>` \"getKey\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** HasMethod :ref:`Election <type-daml-finance-interface-lifecycle-v4-election-election-99800>` \"apply\" (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Election <type-daml-finance-interface-lifecycle-v4-election-election-99800>` \-\> :ref:`Apply <type-daml-finance-interface-lifecycle-v4-election-apply-6828>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\]))

  **instance** HasMethod :ref:`Exercisable <type-daml-finance-interface-lifecycle-v4-election-exercisable-36259>` \"applyElection\" (:ref:`ApplyElection <type-daml-finance-interface-lifecycle-v4-election-applyelection-69809>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\]))

  **instance** HasMethod :ref:`Lifecycle <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-lifecycle-50587>` \"evolve\" (:ref:`Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\]))

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ GetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494>`)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ SetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ SetObservers (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

.. _type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264:

**type** `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_
  \= `Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _type-daml-finance-interface-types-common-v3-types-parties-67059:

**type** `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_
  \= `Set <https://docs.daml.com/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  A set of parties\.

.. _type-daml-finance-interface-types-common-v3-types-partiesmap-43006:

**type** `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_
  \= `Map <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  Parties mapped by a specific key (or context)\.
  The textual key is the \"context\" which describes the value set of parties\.
  This allows processes to add/remove parties for their specific purpose, without affecting others\.

.. _type-daml-finance-interface-types-common-v3-types-quantity-28585:

**data** `Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a

  A dimensioned quantity\.

  .. _constr-daml-finance-interface-types-common-v3-types-quantity-96274:

  `Quantity <constr-daml-finance-interface-types-common-v3-types-quantity-96274_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - unit
         - u
         - The quantity's unit\.
       * - amount
         - a
         - A numerical amount\.

  **instance** (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ u, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a) \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)

  **instance** (`Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ u, `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ a) \=\> `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)

  **instance** (`Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ u, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a) \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)
