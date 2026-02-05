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

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"custodian\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"owner\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"custodian\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"owner\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

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

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"provider\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"provider\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

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

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

.. _type-daml-finance-interface-types-common-v3-types-id-28519:

**data** `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  .. _constr-daml-finance-interface-types-common-v3-types-id-84864:

  `Id <constr-daml-finance-interface-types-common-v3-types-id-84864_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_


  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

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

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"depository\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holdingStandard\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"issuer\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"version\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"depository\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holdingStandard\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Id <type-daml-finance-interface-types-common-v3-types-id-28519_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"issuer\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"version\" `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

.. _type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264:

**type** `InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264_>`_
  \= `Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ `InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _type-daml-finance-interface-types-common-v3-types-parties-67059:

**type** `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_
  \= `Set <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  A set of parties\.

.. _type-daml-finance-interface-types-common-v3-types-partiesmap-43006:

**type** `PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006_>`_
  \= `Map <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ `Parties <type-daml-finance-interface-types-common-v3-types-parties-67059_>`_

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

  **instance** (`Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ u, `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a) \=\> `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)

  **instance** (`Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ u, `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ a) \=\> `Ord <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)

  **instance** (`Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ u, `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a) \=\> `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"amount\" (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a) a

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"unit\" (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a) u

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"amount\" (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a) a

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"unit\" (`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585_>`_ u a) u
