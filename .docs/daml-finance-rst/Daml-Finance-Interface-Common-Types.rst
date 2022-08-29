.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-common-types-89891:

Module Daml.Finance.Interface.Common.Types
==========================================

Data Types
----------

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
