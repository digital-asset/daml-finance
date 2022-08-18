.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-types-46191:

Module Daml.Finance.Interface.Asset.Types
=========================================

Data Types
----------

.. _type-daml-finance-interface-asset-types-accountkey-21197:

**data** `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_

  A unique key for Accounts\.
  
  .. _constr-daml-finance-interface-asset-types-accountkey-86318:
  
  `AccountKey <constr-daml-finance-interface-asset-types-accountkey-86318_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - custodian
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing accounting services\.
       * - owner
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing accounting services\.
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Identifier of the account\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** HasMethod :ref:`Account <type-daml-finance-interface-asset-account-account-19971>` \"getKey\" `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_ GetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Account <type-daml-finance-interface-asset-account-account-19971>`)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_ SetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_ SetObservers (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()
  
  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_
  
  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `AccountKey <type-daml-finance-interface-asset-types-accountkey-21197_>`_

.. _type-daml-finance-interface-asset-types-id-89116:

**data** `Id <type-daml-finance-interface-asset-types-id-89116_>`_

  An identifier including a textual label and a textual version\.
  ``Text`` is used for the version as the target set might not have a total order\.
  
  .. _constr-daml-finance-interface-asset-types-id-49147:
  
  `Id <constr-daml-finance-interface-asset-types-id-49147_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - label
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual label\.
       * - version
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual version\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Id <type-daml-finance-interface-asset-types-id-89116_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `Id <type-daml-finance-interface-asset-types-id-89116_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Id <type-daml-finance-interface-asset-types-id-89116_>`_

.. _type-daml-finance-interface-asset-types-instrumentkey-68480:

**data** `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_

  A unique key for Instruments\.
  
  .. _constr-daml-finance-interface-asset-types-instrumentkey-3353:
  
  `InstrumentKey <constr-daml-finance-interface-asset-types-instrumentkey-3353_>`_
  
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
         - `Id <type-daml-finance-interface-asset-types-id-89116_>`_
         - Identifier of the instrument\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-asset-instrument-instrument-30765>` \"getKey\" `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_ GetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Instrument <type-daml-finance-interface-asset-instrument-instrument-30765>`)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_ SetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_ SetObservers (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()
  
  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_
  
  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480_>`_

.. _type-daml-finance-interface-asset-types-quantity-64806:

**data** `Quantity <type-daml-finance-interface-asset-types-quantity-64806_>`_ u a

  A dimensioned quantity\.
  
  .. _constr-daml-finance-interface-asset-types-quantity-79157:
  
  `Quantity <constr-daml-finance-interface-asset-types-quantity-79157_>`_
  
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
  
  **instance** (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ u, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a) \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Quantity <type-daml-finance-interface-asset-types-quantity-64806_>`_ u a)
  
  **instance** (`Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ u, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a) \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Quantity <type-daml-finance-interface-asset-types-quantity-64806_>`_ u a)
