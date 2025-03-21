.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-settlement-v4-types-65938:

Daml.Finance.Interface.Settlement.V4.Types
==========================================

Data Types
----------

.. _type-daml-finance-interface-settlement-v4-types-allocation-41200:

**data** `Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200_>`_

  Describes an allocation of an ``Instruction``\.

  .. _constr-daml-finance-interface-settlement-v4-types-unallocated-28244:

  `Unallocated <constr-daml-finance-interface-settlement-v4-types-unallocated-28244_>`_

    An unallocated instruction\.

  .. _constr-daml-finance-interface-settlement-v4-types-pledge-57866:

  `Pledge <constr-daml-finance-interface-settlement-v4-types-pledge-57866_>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`)

    Settle the instruction with the pledged asset\.

  .. _constr-daml-finance-interface-settlement-v4-types-creditreceiver-33781:

  `CreditReceiver <constr-daml-finance-interface-settlement-v4-types-creditreceiver-33781_>`_

    Settle the instruction by crediting the receiver account (where the sender is custodian)\.

  .. _constr-daml-finance-interface-settlement-v4-types-settleoffledger-82795:

  `SettleOffledger <constr-daml-finance-interface-settlement-v4-types-settleoffledger-82795_>`_

    Settle the instruction off\-ledger\.

  .. _constr-daml-finance-interface-settlement-v4-types-passthroughfrom-2474:

  `PassThroughFrom <constr-daml-finance-interface-settlement-v4-types-passthroughfrom-2474_>`_ (:ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`, `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_)

    Settle the instruction with the holding coming from the specified instruction and account\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200_>`_

.. _type-daml-finance-interface-settlement-v4-types-approval-77821:

**data** `Approval <type-daml-finance-interface-settlement-v4-types-approval-77821_>`_

  Describes an approval of an ``Instruction``\.

  .. _constr-daml-finance-interface-settlement-v4-types-unapproved-58597:

  `Unapproved <constr-daml-finance-interface-settlement-v4-types-unapproved-58597_>`_

    An unapproved instruction\.

  .. _constr-daml-finance-interface-settlement-v4-types-takedelivery-31030:

  `TakeDelivery <constr-daml-finance-interface-settlement-v4-types-takedelivery-31030_>`_ :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`

    Take delivery to the specified account\.

  .. _constr-daml-finance-interface-settlement-v4-types-debitsender-18125:

  `DebitSender <constr-daml-finance-interface-settlement-v4-types-debitsender-18125_>`_

    Debit the sender account with the provided asset (where the receiver is custodian)\.

  .. _constr-daml-finance-interface-settlement-v4-types-settleoffledgeracknowledge-65556:

  `SettleOffledgerAcknowledge <constr-daml-finance-interface-settlement-v4-types-settleoffledgeracknowledge-65556_>`_

    Acknowledge settlement of the instruction off\-ledger\.

  .. _constr-daml-finance-interface-settlement-v4-types-passthroughto-8399:

  `PassThroughTo <constr-daml-finance-interface-settlement-v4-types-passthroughto-8399_>`_ (:ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`, `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_)

    Take delivery to the specified account\. The holding is then immediately allocated to the
    specified instruction\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Approval <type-daml-finance-interface-settlement-v4-types-approval-77821_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Approval <type-daml-finance-interface-settlement-v4-types-approval-77821_>`_

.. _type-daml-finance-interface-settlement-v4-types-instructionkey-88375:

**data** `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  A unique key for Instructions\.

  .. _constr-daml-finance-interface-settlement-v4-types-instructionkey-89360:

  `InstructionKey <constr-daml-finance-interface-settlement-v4-types-instructionkey-89360_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instructor
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party instructing settlement (and the creation of the ``Instruction``)\.
       * - batchId
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Id of the batch the instruction belongs to\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - A unique identifier for an instruction\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ :ref:`Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077>` `InstructionKey <type-daml-finance-interface-settlement-v4-types-instructionkey-88375_>`_

.. _type-daml-finance-interface-settlement-v4-types-routedstep-26293:

**data** `RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293_>`_

  Describes a transfer of a position between two parties\.
  The custodian at which the position is held is also specified\.

  .. _constr-daml-finance-interface-settlement-v4-types-routedstep-5658:

  `RoutedStep <constr-daml-finance-interface-settlement-v4-types-routedstep-5658_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - sender
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party transferring the asset\.
       * - receiver
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party receiving the asset\.
       * - custodian
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The custodian at which the asset is held\.
       * - quantity
         - :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
         - The instrument and amount to be transferred\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293_>`_

  **instance** HasMethod :ref:`RouteProvider <type-daml-finance-interface-settlement-v4-routeprovider-routeprovider-29628>` \"discover\" (:ref:`Discover <type-daml-finance-interface-settlement-v4-routeprovider-discover-692>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293_>`_\])

.. _type-daml-finance-interface-settlement-v4-types-step-16302:

**data** `Step <type-daml-finance-interface-settlement-v4-types-step-16302_>`_

  Describes a transfer of a position between two parties\.

  .. _constr-daml-finance-interface-settlement-v4-types-step-28201:

  `Step <constr-daml-finance-interface-settlement-v4-types-step-28201_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - sender
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party transferring the asset\.
       * - receiver
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party receiving the asset\.
       * - quantity
         - :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
         - The instrument and amount to be transferred\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Step <type-daml-finance-interface-settlement-v4-types-step-16302_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `Step <type-daml-finance-interface-settlement-v4-types-step-16302_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Step <type-daml-finance-interface-settlement-v4-types-step-16302_>`_
