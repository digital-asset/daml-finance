.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-common-types-89891:

Module Daml.Finance.Interface.Common.Types
==========================================

Data Types
----------

.. _type-daml-finance-interface-common-types-accountkey-81709:

**data** `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_

  A unique key for Accounts\.
  
  .. _constr-daml-finance-interface-common-types-accountkey-20268:
  
  `AccountKey <constr-daml-finance-interface-common-types-accountkey-20268_>`_
  
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
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** HasMethod :ref:`Account <type-daml-finance-interface-asset-account-account-19971>` \"getKey\" `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_ GetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Account <type-daml-finance-interface-asset-account-account-19971>`)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_ SetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_ SetObservers (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()
  
  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_
  
  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `AccountKey <type-daml-finance-interface-common-types-accountkey-81709_>`_

.. _type-daml-finance-interface-common-types-id-88316:

**data** `Id <type-daml-finance-interface-common-types-id-88316_>`_

  An identifier including a textual label and a textual version\.
  ``Text`` is used for the version as the target set might not have a total order\.
  
  .. _constr-daml-finance-interface-common-types-id-17333:
  
  `Id <constr-daml-finance-interface-common-types-id-17333_>`_
  
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
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Id <type-daml-finance-interface-common-types-id-88316_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `Id <type-daml-finance-interface-common-types-id-88316_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Id <type-daml-finance-interface-common-types-id-88316_>`_

.. _type-daml-finance-interface-common-types-instrumentkey-87168:

**data** `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_

  A unique key for Instruments\.
  
  .. _constr-daml-finance-interface-common-types-instrumentkey-35847:
  
  `InstrumentKey <constr-daml-finance-interface-common-types-instrumentkey-35847_>`_
  
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
         - `Id <type-daml-finance-interface-common-types-id-88316_>`_
         - Identifier of the instrument\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-instrument-base-instrument-instrument-22935>` \"getKey\" `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_ GetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Instrument <type-daml-finance-interface-instrument-base-instrument-instrument-22935>`)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_ SetCid (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_ SetObservers (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Reference)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()
  
  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_
  
  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ Reference `InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168_>`_

.. _type-daml-finance-interface-common-types-observers-20361:

**type** `Observers <type-daml-finance-interface-common-types-observers-20361_>`_
  \= `Map <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ (`Set <https://docs.daml.com/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Parties <type-daml-finance-interface-common-types-parties-45858_>`_)
  
  Identifies observers of a contract\. The textual key is the \"context\" under which the parties were added as observers\.

.. _type-daml-finance-interface-common-types-parties-45858:

**type** `Parties <type-daml-finance-interface-common-types-parties-45858_>`_
  \= `Set <https://docs.daml.com/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
  
  A set of parties\.
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` (`Parties <type-daml-finance-interface-common-types-parties-45858_>`_, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_) `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()
  
  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` (`Parties <type-daml-finance-interface-common-types-parties-45858_>`_, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)
  
  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` (`Parties <type-daml-finance-interface-common-types-parties-45858_>`_, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)
  
  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` (`Parties <type-daml-finance-interface-common-types-parties-45858_>`_, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)
  
  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` (`Parties <type-daml-finance-interface-common-types-parties-45858_>`_, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)
  
  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` (`Parties <type-daml-finance-interface-common-types-parties-45858_>`_, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)
  
  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` (`Parties <type-daml-finance-interface-common-types-parties-45858_>`_, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)

.. _type-daml-finance-interface-common-types-quantity-934:

**data** `Quantity <type-daml-finance-interface-common-types-quantity-934_>`_ u a

  A dimensioned quantity\.
  
  .. _constr-daml-finance-interface-common-types-quantity-26687:
  
  `Quantity <constr-daml-finance-interface-common-types-quantity-26687_>`_
  
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
  
  **instance** (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ u, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a) \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Quantity <type-daml-finance-interface-common-types-quantity-934_>`_ u a)
  
  **instance** (`Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ u, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a) \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Quantity <type-daml-finance-interface-common-types-quantity-934_>`_ u a)
