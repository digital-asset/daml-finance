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
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Account provider\.
       * - owner
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Account owner\.
       * - id
         - `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_
         - Unique identifier for an account\.

  **instance** HasInterfaceKey :ref:`Account <type-daml-finance-interface-account-v4-account-account-93407>` :ref:`View <type-daml-finance-interface-account-v4-account-view-18066>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ Reference GetCid SetCid SetObservers :ref:`GetView <type-daml-finance-interface-account-v4-account-getview-21073>`

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** HasMethod :ref:`Account <type-daml-finance-interface-account-v4-account-account-93407>` \"getKey\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"account\" :ref:`BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"account\" :ref:`Fungible <type-daml-finance-holding-v4-fungible-fungible-67336>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"account\" :ref:`Transferable <type-daml-finance-holding-v4-transferable-transferable-12222>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"account\" :ref:`TransferableFungible <type-daml-finance-holding-v4-transferablefungible-transferablefungible-50906>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"account\" :ref:`Create <type-daml-finance-interface-account-v4-factory-create-72130>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"account\" :ref:`Create <type-daml-finance-interface-holding-v4-factory-create-84550>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"account\" :ref:`View <type-daml-finance-interface-holding-v4-holding-view-10906>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"custodian\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newOwnerAccount\" :ref:`Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"owner\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"account\" :ref:`BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"account\" :ref:`Fungible <type-daml-finance-holding-v4-fungible-fungible-67336>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"account\" :ref:`Transferable <type-daml-finance-holding-v4-transferable-transferable-12222>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"account\" :ref:`TransferableFungible <type-daml-finance-holding-v4-transferablefungible-transferablefungible-50906>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"account\" :ref:`Create <type-daml-finance-interface-account-v4-factory-create-72130>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"account\" :ref:`Create <type-daml-finance-interface-holding-v4-factory-create-84550>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"account\" :ref:`View <type-daml-finance-interface-holding-v4-holding-view-10906>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"custodian\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newOwnerAccount\" :ref:`Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593>` `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"owner\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ GetCid (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Account <type-daml-finance-interface-account-v4-account-account-93407>`)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ SetCid (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ SetObservers (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Archive <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasFromAnyContractKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasLookupByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasMaintainer <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `HasToAnyContractKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

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
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Holding factory provider\.
       * - id
         - `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_
         - Unique identifier for a holding factory\.

  **instance** HasInterfaceKey :ref:`Factory <type-daml-finance-interface-holding-v4-factory-factory-22859>` :ref:`View <type-daml-finance-interface-holding-v4-factory-view-66511>` `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ Reference GetCid SetCid SetObservers :ref:`GetView <type-daml-finance-interface-holding-v4-factory-getview-97414>`

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-holding-v4-factory-factory-22859>` \"getKey\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingFactory\" :ref:`Account <type-daml-finance-account-v4-account-account-35720>` `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingFactory\" :ref:`Create <type-daml-finance-interface-account-v4-factory-create-72130>` `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"provider\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingFactory\" :ref:`Account <type-daml-finance-account-v4-account-account-35720>` `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingFactory\" :ref:`Create <type-daml-finance-interface-account-v4-factory-create-72130>` `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"provider\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ GetCid (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Factory <type-daml-finance-interface-holding-v4-factory-factory-22859>`)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ SetCid (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ SetObservers (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Archive <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasFromAnyContractKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasLookupByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasMaintainer <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `HasToAnyContractKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

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

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-zerocoupon-instrument-instrument-10035>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" :ref:`View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-zerocoupon-instrument-instrument-10035>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" :ref:`View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900>` `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

.. _type-daml-finance-interface-types-common-v3-types-id-28519:

**data** `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  .. _constr-daml-finance-interface-types-common-v3-types-id-84864:

  `Id <constr-daml-finance-interface-types-common-v3-types-id-84864_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_


  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"batchId\" :ref:`ClaimEffect <type-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-78754>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"batchId\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"batchId\" :ref:`InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"batchId\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"contextId\" :ref:`View <type-daml-finance-interface-settlement-v4-batch-view-11618>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"contextId\" :ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"contextId\" :ref:`Discover <type-daml-finance-interface-settlement-v4-routeprovider-discover-692>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"contextId\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"eventId\" :ref:`Advance <type-daml-finance-interface-data-v4-reference-time-advance-64582>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"eventId\" :ref:`Rewind <type-daml-finance-interface-data-v4-reference-time-rewind-93104>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Account <type-daml-finance-account-v4-account-account-35720>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Observation <type-daml-finance-data-v4-numeric-observation-observation-13815>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Factory <type-daml-finance-holding-v4-factory-factory-39768>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-zerocoupon-instrument-instrument-10035>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Rule <type-daml-finance-instrument-generic-v4-lifecycle-rule-rule-21784>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-account-v4-account-view-18066>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Create <type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-data-v4-numeric-observation-view-99464>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-data-v4-reference-time-view-8124>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-holding-v4-factory-view-66511>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`DeclareReplacement <type-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-46147>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`DeclareStockSplit <type-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-89514>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-election-view-84858>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-view-53912>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-29492>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-1867>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-settlement-v4-batch-view-11618>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Election <type-daml-finance-lifecycle-v4-election-election-87911>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"routedStepsWithInstructionId\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` \[(:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"batchId\" :ref:`ClaimEffect <type-daml-finance-interface-lifecycle-v4-rule-claim-claimeffect-78754>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"batchId\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"batchId\" :ref:`InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"batchId\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"contextId\" :ref:`View <type-daml-finance-interface-settlement-v4-batch-view-11618>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"contextId\" :ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"contextId\" :ref:`Discover <type-daml-finance-interface-settlement-v4-routeprovider-discover-692>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"contextId\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"eventId\" :ref:`Advance <type-daml-finance-interface-data-v4-reference-time-advance-64582>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"eventId\" :ref:`Rewind <type-daml-finance-interface-data-v4-reference-time-rewind-93104>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Account <type-daml-finance-account-v4-account-account-35720>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Observation <type-daml-finance-data-v4-numeric-observation-observation-13815>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Factory <type-daml-finance-holding-v4-factory-factory-39768>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-zerocoupon-instrument-instrument-10035>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Rule <type-daml-finance-instrument-generic-v4-lifecycle-rule-rule-21784>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-account-v4-account-view-18066>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Create <type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-data-v4-numeric-observation-view-99464>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-data-v4-reference-time-view-8124>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-holding-v4-factory-view-66511>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`DeclareReplacement <type-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-46147>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`DeclareStockSplit <type-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-89514>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-election-view-84858>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-view-53912>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-29492>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-1867>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-settlement-v4-batch-view-11618>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Election <type-daml-finance-lifecycle-v4-election-election-87911>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"routedStepsWithInstructionId\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` \[(:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)\]

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_) `Archive <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `HasFromAnyContractKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `HasKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `HasLookupByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `HasMaintainer <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

  **instance** `HasToAnyContractKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` (`Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_, `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_)

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
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing depository services\.
       * - issuer
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Issuer of instrument\.
       * - id
         - `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_
         - A unique identifier for an instrument\.
       * - version
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual instrument version\.
       * - holdingStandard
         - `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_
         - The used holding standard for the instrument\.

  **instance** HasInterfaceKey :ref:`Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494>` :ref:`View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ Reference GetCid SetCid SetObservers :ref:`GetView <type-daml-finance-interface-instrument-base-v4-instrument-getview-66559>`

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494>` \"getKey\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** HasMethod :ref:`Election <type-daml-finance-interface-lifecycle-v4-election-election-99800>` \"apply\" (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Election <type-daml-finance-interface-lifecycle-v4-election-election-99800>` \-\> :ref:`Apply <type-daml-finance-interface-lifecycle-v4-election-apply-6828>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_, \[`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\]))

  **instance** HasMethod :ref:`Exercisable <type-daml-finance-interface-lifecycle-v4-election-exercisable-36259>` \"applyElection\" (:ref:`ApplyElection <type-daml-finance-interface-lifecycle-v4-election-applyelection-69809>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_, \[`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\]))

  **instance** HasMethod :ref:`Lifecycle <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-lifecycle-50587>` \"evolve\" (:ref:`Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_, \[`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\]))

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"baseCurrency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"baseCurrency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"baseCurrency\" :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"baseCurrency\" :ref:`ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currencies\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235>` \[`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currencies\" :ref:`Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949>` \[`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-zerocoupon-instrument-instrument-10035>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Callable <type-daml-finance-interface-instrument-bond-v3-callable-types-callable-12794>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`FixedRate <type-daml-finance-interface-instrument-bond-v3-fixedrate-types-fixedrate-8592>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`FloatingRate <type-daml-finance-interface-instrument-bond-v3-floatingrate-types-floatingrate-91442>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" :ref:`InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"depository\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignCurrency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignCurrency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignCurrency\" :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignCurrency\" :ref:`ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Fungible <type-daml-finance-holding-v4-fungible-fungible-67336>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Transferable <type-daml-finance-holding-v4-transferable-transferable-12222>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`TransferableFungible <type-daml-finance-holding-v4-transferablefungible-transferablefungible-50906>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-holding-v4-factory-create-84550>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`View <type-daml-finance-interface-holding-v4-holding-view-10906>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Callable <type-daml-finance-interface-instrument-bond-v3-callable-types-callable-12794>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`FixedRate <type-daml-finance-interface-instrument-bond-v3-fixedrate-types-fixedrate-8592>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`FloatingRate <type-daml-finance-interface-instrument-bond-v3-floatingrate-types-floatingrate-91442>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-instrument-equity-v0-factory-create-45111>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`View <type-daml-finance-interface-instrument-equity-v0-instrument-view-97536>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-instrument-generic-v4-factory-create-52332>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`View <type-daml-finance-interface-instrument-generic-v4-instrument-view-70325>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Token <type-daml-finance-interface-instrument-token-v4-types-token-51711>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-election-view-84858>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Election <type-daml-finance-lifecycle-v4-election-election-87911>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" :ref:`Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"issuer\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newInstrument\" :ref:`DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newInstrument\" :ref:`DeclareStockSplit <type-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-89514>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newInstrument\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"producedInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"producedInstrument\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"producedInstrument\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"quantity\" :ref:`Credit <type-daml-finance-interface-account-v4-account-credit-92816>` (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"referenceAsset\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"referenceAsset\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"referenceAsset\" :ref:`Underlying <type-daml-finance-interface-instrument-swap-v0-asset-types-underlying-93813>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"targetInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"targetInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"targetInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-replacement-view-74170>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"targetInstrument\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"targetInstrument\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"targetInstrument\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"targetInstrument\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"version\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"baseCurrency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"baseCurrency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"baseCurrency\" :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"baseCurrency\" :ref:`ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currencies\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235>` \[`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currencies\" :ref:`Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949>` \[`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-zerocoupon-instrument-instrument-10035>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Callable <type-daml-finance-interface-instrument-bond-v3-callable-types-callable-12794>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`FixedRate <type-daml-finance-interface-instrument-bond-v3-fixedrate-types-fixedrate-8592>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`FloatingRate <type-daml-finance-interface-instrument-bond-v3-floatingrate-types-floatingrate-91442>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" :ref:`InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"depository\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignCurrency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignCurrency\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignCurrency\" :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignCurrency\" :ref:`ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Fungible <type-daml-finance-holding-v4-fungible-fungible-67336>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Transferable <type-daml-finance-holding-v4-transferable-transferable-12222>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`TransferableFungible <type-daml-finance-holding-v4-transferablefungible-transferablefungible-50906>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-holding-v4-factory-create-84550>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`View <type-daml-finance-interface-holding-v4-holding-view-10906>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Callable <type-daml-finance-interface-instrument-bond-v3-callable-types-callable-12794>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`FixedRate <type-daml-finance-interface-instrument-bond-v3-fixedrate-types-fixedrate-8592>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`FloatingRate <type-daml-finance-interface-instrument-bond-v3-floatingrate-types-floatingrate-91442>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-instrument-equity-v0-factory-create-45111>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`View <type-daml-finance-interface-instrument-equity-v0-instrument-view-97536>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-instrument-generic-v4-factory-create-52332>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`View <type-daml-finance-interface-instrument-generic-v4-instrument-view-70325>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Token <type-daml-finance-interface-instrument-token-v4-types-token-51711>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-election-view-84858>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Election <type-daml-finance-lifecycle-v4-election-election-87911>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" :ref:`Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"issuer\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newInstrument\" :ref:`DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newInstrument\" :ref:`DeclareStockSplit <type-daml-finance-interface-instrument-equity-v0-instrument-declarestocksplit-89514>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newInstrument\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"producedInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"producedInstrument\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"producedInstrument\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"quantity\" :ref:`Credit <type-daml-finance-interface-account-v4-account-credit-92816>` (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"referenceAsset\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"referenceAsset\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"referenceAsset\" :ref:`Underlying <type-daml-finance-interface-instrument-swap-v0-asset-types-underlying-93813>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"targetInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"targetInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"targetInstrument\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-replacement-view-74170>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"targetInstrument\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"targetInstrument\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"targetInstrument\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"targetInstrument\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"version\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ GetCid (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494>`)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ SetCid (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ SetObservers (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)

  **instance** `HasExerciseByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Archive <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasFromAnyContractKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasLookupByKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasMaintainer <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `HasToAnyContractKey <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

.. _type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264:

**type** `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_
  \= `Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"cashQuantity\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"cashQuantity\" :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>` `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"consumed\" :ref:`CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"fxQuantity\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"fxQuantity\" :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"otherConsumed\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"otherConsumed\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"otherConsumed\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"otherProduced\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"otherProduced\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"otherProduced\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"perUnitDistribution\" :ref:`DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"perUnitDistribution\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"perUnitDistribution\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"perUnitReplacement\" :ref:`DeclareReplacement <type-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-46147>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"perUnitReplacement\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-replacement-view-74170>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"perUnitReplacement\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"produced\" :ref:`CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"quantity\" :ref:`Calculate <type-daml-finance-interface-lifecycle-v4-effect-calculate-57344>` `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"sharesQuantity\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"sharesQuantity\" :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"cashQuantity\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"cashQuantity\" :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>` `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"consumed\" :ref:`CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"fxQuantity\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"fxQuantity\" :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"otherConsumed\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"otherConsumed\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"otherConsumed\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"otherProduced\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"otherProduced\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"otherProduced\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"perUnitDistribution\" :ref:`DeclareDistribution <type-daml-finance-interface-instrument-equity-v0-instrument-declaredistribution-57612>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"perUnitDistribution\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"perUnitDistribution\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"perUnitReplacement\" :ref:`DeclareReplacement <type-daml-finance-interface-instrument-equity-v0-instrument-declarereplacement-46147>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"perUnitReplacement\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-replacement-view-74170>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"perUnitReplacement\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"produced\" :ref:`CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392>` \[`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"quantity\" :ref:`Calculate <type-daml-finance-interface-lifecycle-v4-effect-calculate-57344>` `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"sharesQuantity\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"sharesQuantity\" :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_)

.. _type-daml-finance-interface-types-common-v3-types-parties-67059:

**type** `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_
  \= `Set <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  A set of parties\.

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Observe <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-observe-90440>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`GetTime <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-gettime-64432>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Cancel <type-daml-finance-interface-settlement-v4-batch-cancel-13653>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Settle <type-daml-finance-interface-settlement-v4-batch-settle-93506>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Allocate <type-daml-finance-interface-settlement-v4-instruction-allocate-48530>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Approve <type-daml-finance-interface-settlement-v4-instruction-approve-69723>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Cancel <type-daml-finance-interface-settlement-v4-instruction-cancel-2291>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"actors\" :ref:`Execute <type-daml-finance-interface-settlement-v4-instruction-execute-24017>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"claimers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"claimers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-claim-rule-66621>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"consenters\" :ref:`View <type-daml-finance-interface-settlement-v4-batch-view-11618>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"consenters\" :ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"consenters\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"consenters\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"consenters\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"controllers\" :ref:`View <type-daml-finance-interface-util-v3-lockable-view-77974>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"controllers\" :ref:`View <type-daml-finance-interface-util-v3-lockablesplice-view-30376>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"disclosers\" :ref:`AddObservers <type-daml-finance-interface-util-v3-disclosure-addobservers-68807>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"disclosers\" :ref:`RemoveObservers <type-daml-finance-interface-util-v3-disclosure-removeobservers-4683>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"disclosers\" :ref:`SetObservers <type-daml-finance-interface-util-v3-disclosure-setobservers-68580>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"disclosureControllers\" :ref:`View <type-daml-finance-interface-util-v3-disclosure-view-81206>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"discoverors\" :ref:`Discover <type-daml-finance-interface-settlement-v4-routeprovider-discover-692>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"incoming\" :ref:`Controllers <type-daml-finance-interface-account-v4-account-controllers-59817>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lockers\" :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lockers\" :ref:`Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"modifiers\" :ref:`View <type-daml-finance-interface-holding-v4-fungible-view-93398>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newInstrumentObservers\" :ref:`DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789>` \[(`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_)\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newLockers\" :ref:`Acquire <type-daml-finance-interface-util-v3-lockable-acquire-20270>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newLockers\" :ref:`Acquire <type-daml-finance-interface-util-v3-lockablesplice-acquire-89216>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newProviders\" :ref:`SetProviders <type-daml-finance-interface-lifecycle-v4-effect-setproviders-39879>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`View <type-daml-finance-interface-settlement-v4-factory-view-31386>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`View <type-daml-finance-interface-settlement-v4-routeprovider-view-58066>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-settlement-v4-factory-factory-91685>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`IntermediatedStatic <type-daml-finance-settlement-v4-routeprovider-intermediatedstatic-intermediatedstatic-19069>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`SingleCustodian <type-daml-finance-settlement-v4-routeprovider-singlecustodian-singlecustodian-38133>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observersToAdd\" :ref:`AddObservers <type-daml-finance-interface-util-v3-disclosure-addobservers-68807>` (`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observersToRemove\" :ref:`RemoveObservers <type-daml-finance-interface-util-v3-disclosure-removeobservers-4683>` (`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"outgoing\" :ref:`Controllers <type-daml-finance-interface-account-v4-account-controllers-59817>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`Rule <type-daml-finance-instrument-generic-v4-lifecycle-rule-rule-21784>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`View <type-daml-finance-interface-data-v4-reference-time-view-8124>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-view-53912>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlers\" :ref:`View <type-daml-finance-interface-settlement-v4-batch-view-11618>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlers\" :ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlers\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-claim-rule-66621>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlers\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlers\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"signedReceivers\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"signedReceivers\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"signedSenders\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"signedSenders\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Observe <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-observe-90440>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`GetTime <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-gettime-64432>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Cancel <type-daml-finance-interface-settlement-v4-batch-cancel-13653>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Settle <type-daml-finance-interface-settlement-v4-batch-settle-93506>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Allocate <type-daml-finance-interface-settlement-v4-instruction-allocate-48530>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Approve <type-daml-finance-interface-settlement-v4-instruction-approve-69723>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Cancel <type-daml-finance-interface-settlement-v4-instruction-cancel-2291>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"actors\" :ref:`Execute <type-daml-finance-interface-settlement-v4-instruction-execute-24017>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"claimers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"claimers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-claim-rule-66621>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"consenters\" :ref:`View <type-daml-finance-interface-settlement-v4-batch-view-11618>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"consenters\" :ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"consenters\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"consenters\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"consenters\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"controllers\" :ref:`View <type-daml-finance-interface-util-v3-lockable-view-77974>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"controllers\" :ref:`View <type-daml-finance-interface-util-v3-lockablesplice-view-30376>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"disclosers\" :ref:`AddObservers <type-daml-finance-interface-util-v3-disclosure-addobservers-68807>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"disclosers\" :ref:`RemoveObservers <type-daml-finance-interface-util-v3-disclosure-removeobservers-4683>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"disclosers\" :ref:`SetObservers <type-daml-finance-interface-util-v3-disclosure-setobservers-68580>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"disclosureControllers\" :ref:`View <type-daml-finance-interface-util-v3-disclosure-view-81206>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"discoverors\" :ref:`Discover <type-daml-finance-interface-settlement-v4-routeprovider-discover-692>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"incoming\" :ref:`Controllers <type-daml-finance-interface-account-v4-account-controllers-59817>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lockers\" :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lockers\" :ref:`Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"modifiers\" :ref:`View <type-daml-finance-interface-holding-v4-fungible-view-93398>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newInstrumentObservers\" :ref:`DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789>` \[(`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_)\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newLockers\" :ref:`Acquire <type-daml-finance-interface-util-v3-lockable-acquire-20270>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newLockers\" :ref:`Acquire <type-daml-finance-interface-util-v3-lockablesplice-acquire-89216>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newProviders\" :ref:`SetProviders <type-daml-finance-interface-lifecycle-v4-effect-setproviders-39879>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`View <type-daml-finance-interface-settlement-v4-factory-view-31386>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`View <type-daml-finance-interface-settlement-v4-routeprovider-view-58066>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-settlement-v4-factory-factory-91685>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`IntermediatedStatic <type-daml-finance-settlement-v4-routeprovider-intermediatedstatic-intermediatedstatic-19069>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`SingleCustodian <type-daml-finance-settlement-v4-routeprovider-singlecustodian-singlecustodian-38133>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observersToAdd\" :ref:`AddObservers <type-daml-finance-interface-util-v3-disclosure-addobservers-68807>` (`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observersToRemove\" :ref:`RemoveObservers <type-daml-finance-interface-util-v3-disclosure-removeobservers-4683>` (`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"outgoing\" :ref:`Controllers <type-daml-finance-interface-account-v4-account-controllers-59817>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`LedgerTime <type-daml-finance-data-v4-time-ledgertime-ledgertime-59708>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`Rule <type-daml-finance-instrument-generic-v4-lifecycle-rule-rule-21784>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`DistributionRule <type-daml-finance-instrument-swap-v0-asset-distributionrule-distributionrule-67789>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`View <type-daml-finance-interface-data-v4-reference-time-view-8124>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-effect-view-53622>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-event-view-53912>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-distribution-rule-34>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-replacement-rule-24043>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-rule-claim-view-14471>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlers\" :ref:`View <type-daml-finance-interface-settlement-v4-batch-view-11618>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlers\" :ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlers\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlers\" :ref:`Rule <type-daml-finance-lifecycle-v4-rule-claim-rule-66621>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlers\" :ref:`Batch <type-daml-finance-settlement-v4-batch-batch-9941>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlers\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"signedReceivers\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"signedReceivers\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"signedSenders\" :ref:`View <type-daml-finance-interface-settlement-v4-instruction-view-97904>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"signedSenders\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

.. _type-daml-finance-interface-types-common-v3-types-partiesmap-43006:

**type** `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_
  \= `Map <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

  Parties mapped by a specific key (or context)\.
  The textual key is the \"context\" which describes the value set of parties\.
  This allows processes to add/remove parties for their specific purpose, without affecting others\.

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newObservers\" SetObservers `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newObservers\" SetObservers `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newObservers\" SetObservers `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"newObservers\" :ref:`SetObservers <type-daml-finance-interface-util-v3-disclosure-setobservers-68580>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Account <type-daml-finance-account-v4-account-account-35720>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-account-v4-account-factory-19307>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-data-v4-numeric-observation-factory-28223>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Observation <type-daml-finance-data-v4-numeric-observation-observation-13815>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-data-v4-reference-holidaycalendar-factory-82307>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-holding-v4-factory-factory-39768>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Fungible <type-daml-finance-holding-v4-fungible-fungible-67336>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Transferable <type-daml-finance-holding-v4-transferable-transferable-12222>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`TransferableFungible <type-daml-finance-holding-v4-transferablefungible-transferablefungible-50906>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-callable-factory-factory-32603>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-fixedrate-factory-factory-38968>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-floatingrate-factory-factory-65043>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-inflationlinked-factory-factory-9487>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-zerocoupon-factory-factory-76497>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-zerocoupon-instrument-instrument-10035>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-equity-v0-factory-factory-99971>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-generic-v4-factory-factory-39836>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Rule <type-daml-finance-instrument-generic-v4-lifecycle-rule-rule-21784>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-barriereuropeancash-factory-factory-62768>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-dividend-election-factory-67569>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-dividend-factory-factory-74369>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-europeancash-factory-factory-42074>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-europeanphysical-factory-factory-40216>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-structuredproduct-v0-autocallable-factory-factory-74669>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-factory-factory-32331>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-asset-factory-factory-64565>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-creditdefault-factory-factory-72519>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-currency-factory-factory-69341>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-foreignexchange-factory-factory-75472>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-fpml-factory-factory-13237>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-interestrate-factory-factory-60524>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-token-v4-factory-factory-83934>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" Reference `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-account-v4-factory-create-72130>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`View <type-daml-finance-interface-data-v4-numeric-observation-factory-view-86852>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`View <type-daml-finance-interface-data-v4-numeric-observation-view-99464>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-create-25637>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-holding-v4-factory-create-84550>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" Reference `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" Reference `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-callable-factory-create-44265>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-create-25182>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-create-37181>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-create-17927>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-create-75151>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-equity-v0-factory-create-45111>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-generic-v4-factory-create-52332>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-factory-create-44747>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-europeancash-factory-create-31274>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-create-41404>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-asset-factory-create-72901>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-create-56287>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-currency-factory-create-28047>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-create-78116>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-token-v4-factory-create-20178>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-election-view-84858>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`View <type-daml-finance-interface-util-v3-disclosure-view-81206>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Election <type-daml-finance-lifecycle-v4-election-election-87911>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Factory <type-daml-finance-lifecycle-v4-election-factory-68585>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observers\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newObservers\" SetObservers `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newObservers\" SetObservers `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newObservers\" SetObservers `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"newObservers\" :ref:`SetObservers <type-daml-finance-interface-util-v3-disclosure-setobservers-68580>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Account <type-daml-finance-account-v4-account-account-35720>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-account-v4-account-factory-19307>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Rule <type-daml-finance-claims-v3-lifecycle-rule-rule-14024>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-data-v4-numeric-observation-factory-28223>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Observation <type-daml-finance-data-v4-numeric-observation-observation-13815>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-data-v4-reference-holidaycalendar-factory-82307>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-holding-v4-factory-factory-39768>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Fungible <type-daml-finance-holding-v4-fungible-fungible-67336>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Transferable <type-daml-finance-holding-v4-transferable-transferable-12222>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`TransferableFungible <type-daml-finance-holding-v4-transferablefungible-transferablefungible-50906>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-callable-factory-factory-32603>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-fixedrate-factory-factory-38968>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-floatingrate-factory-factory-65043>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-inflationlinked-factory-factory-9487>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-bond-v3-zerocoupon-factory-factory-76497>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-zerocoupon-instrument-instrument-10035>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-equity-v0-factory-factory-99971>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-equity-v0-instrument-instrument-32561>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-generic-v4-factory-factory-39836>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-generic-v4-instrument-instrument-96378>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Rule <type-daml-finance-instrument-generic-v4-lifecycle-rule-rule-21784>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-barriereuropeancash-factory-factory-62768>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-dividend-election-factory-67569>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-dividend-factory-factory-74369>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-option-v0-dividend-instrument-instrument-69507>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-europeancash-factory-factory-42074>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-option-v0-europeanphysical-factory-factory-40216>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-structuredproduct-v0-autocallable-factory-factory-74669>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-factory-factory-32331>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-asset-factory-factory-64565>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-creditdefault-factory-factory-72519>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-currency-factory-factory-69341>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-foreignexchange-factory-factory-75472>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-foreignexchange-instrument-instrument-3514>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-fpml-factory-factory-13237>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-fpml-instrument-instrument-27235>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-swap-v0-interestrate-factory-factory-60524>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-instrument-token-v4-factory-factory-83934>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instrument <type-daml-finance-instrument-token-v4-instrument-instrument-45256>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" Reference `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-account-v4-factory-create-72130>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`View <type-daml-finance-interface-data-v4-numeric-observation-factory-view-86852>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`View <type-daml-finance-interface-data-v4-numeric-observation-view-99464>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-create-25637>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-holding-v4-factory-create-84550>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" Reference `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" Reference `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-callable-factory-create-44265>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-create-25182>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-create-37181>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-create-17927>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-create-75151>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-equity-v0-factory-create-45111>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-generic-v4-factory-create-52332>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-factory-create-44747>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-europeancash-factory-create-31274>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-create-41404>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-asset-factory-create-72901>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-create-56287>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-currency-factory-create-28047>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-create-78116>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-instrument-token-v4-factory-create-20178>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`View <type-daml-finance-interface-lifecycle-v4-election-view-84858>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`View <type-daml-finance-interface-util-v3-disclosure-view-81206>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Effect <type-daml-finance-lifecycle-v4-effect-effect-15931>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Election <type-daml-finance-lifecycle-v4-election-election-87911>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Factory <type-daml-finance-lifecycle-v4-election-factory-68585>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`ElectionEffect <type-daml-finance-lifecycle-v4-electioneffect-electioneffect-55949>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observers\" :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_

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

  **instance** (`Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ u, `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a) \=\> `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)

  **instance** (`Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ u, `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ a) \=\> `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)

  **instance** (`Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ u, `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a) \=\> `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"amount\" (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a) a

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"quantity\" :ref:`Credit <type-daml-finance-interface-account-v4-account-credit-92816>` (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"unit\" (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a) u

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"amount\" (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a) a

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"quantity\" :ref:`Credit <type-daml-finance-interface-account-v4-account-credit-92816>` (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"unit\" (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a) u
