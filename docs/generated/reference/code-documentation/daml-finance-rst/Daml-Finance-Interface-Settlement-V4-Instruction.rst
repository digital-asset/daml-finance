.. Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-settlement-v4-instruction-71097:

Daml.Finance.Interface.Settlement.V4.Instruction
================================================

Interfaces
----------

.. _type-daml-finance-interface-settlement-v4-instruction-instruction-59168:

**interface** `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_

  An interface for providing a single instruction to transfer an asset\.

  **viewtype** `V <type-daml-finance-interface-settlement-v4-instruction-v-93860_>`_

  + .. _type-daml-finance-interface-settlement-v4-instruction-allocate-48530:

    **Choice** `Allocate <type-daml-finance-interface-settlement-v4-instruction-allocate-48530_>`_

    Allocates this instruction and optionally returns a previously allocated (mutated) asset\.

    Controller\: actors

    Returns\: (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_, `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding))

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The parties allocating the instruction\.
       * - allocation
         - :ref:`Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200>`
         - Allocation of an instruction\.

  + .. _type-daml-finance-interface-settlement-v4-instruction-approve-69723:

    **Choice** `Approve <type-daml-finance-interface-settlement-v4-instruction-approve-69723_>`_

    Approves this instruction\.

    Controller\: actors

    Returns\: `ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The parties approving the instruction\.
       * - approval
         - :ref:`Approval <type-daml-finance-interface-settlement-v4-types-approval-77821>`
         - Approval of an instruction\.

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-settlement-v4-instruction-cancel-2291:

    **Choice** `Cancel <type-daml-finance-interface-settlement-v4-instruction-cancel-2291_>`_

    Cancels this instruction\.

    Controller\: actors

    Returns\: `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding)

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The parties canceling the instruction\.

  + .. _type-daml-finance-interface-settlement-v4-instruction-execute-24017:

    **Choice** `Execute <type-daml-finance-interface-settlement-v4-instruction-execute-24017_>`_

    Executes this instruction\.

    Controller\: actors

    Returns\: `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding)

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The parties executing the instruction\.

  + .. _type-daml-finance-interface-settlement-v4-instruction-getview-68731:

    **Choice** `GetView <type-daml-finance-interface-settlement-v4-instruction-getview-68731_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method allocate \:** `Allocate <type-daml-finance-interface-settlement-v4-instruction-allocate-48530_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_, `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding))

    Implementation of the ``Allocate`` choice\.

  + **Method approve \:** `Approve <type-daml-finance-interface-settlement-v4-instruction-approve-69723_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_)

    Implementation of the ``Approve`` choice\.

  + **Method cancel \:** `Cancel <type-daml-finance-interface-settlement-v4-instruction-cancel-2291_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding))

    Implementation of the ``Cancel`` choice\.

  + **Method execute \:** `Execute <type-daml-finance-interface-settlement-v4-instruction-execute-24017_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding))

    Implementation of the ``Execute`` choice\.

Data Types
----------

.. _type-daml-finance-interface-settlement-v4-instruction-i-65587:

**type** `I <type-daml-finance-interface-settlement-v4-instruction-i-65587_>`_
  \= `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_

  Type synonym for ``Instruction``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-settlement-v4-factory-factory-87188>` \"instruct\" (:ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-batch-i-86753>`, \[`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-settlement-v4-instruction-i-65587_>`_\]))

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instructionCids\" :ref:`ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226>` \[`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-settlement-v4-instruction-i-65587_>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instructionCids\" :ref:`ClaimResult <type-daml-finance-interface-lifecycle-v4-rule-claim-claimresult-10226>` \[`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-settlement-v4-instruction-i-65587_>`_\]

.. _type-daml-finance-interface-settlement-v4-instruction-v-93860:

**type** `V <type-daml-finance-interface-settlement-v4-instruction-v-93860_>`_
  \= `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_ `V <type-daml-finance-interface-settlement-v4-instruction-v-93860_>`_

.. _type-daml-finance-interface-settlement-v4-instruction-view-97904:

**data** `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_

  View for ``Instruction``\.

  .. _constr-daml-finance-interface-settlement-v4-instruction-view-99059:

  `View <constr-daml-finance-interface-settlement-v4-instruction-view-99059_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instructor
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party that instructs settlement (and the creation of the ``Instruction``)\.
       * - consenters
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties consenting with the creation of the ``Instruction``\.
       * - settlers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties that can execute the Instruction\.
       * - batchId
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Batch identifier\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Instruction identifier\.
       * - routedStep
         - :ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`
         - Instruction details to execute\.
       * - settlementTime
         - `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Settlement time (if any)\.
       * - allocation
         - :ref:`Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200>`
         - Allocation from the sender\.
       * - approval
         - :ref:`Approval <type-daml-finance-interface-settlement-v4-types-approval-77821>`
         - Approval from the receiver\.
       * - signedSenders
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Additional signatories, used to collect authorization (on sending side)\.
       * - signedReceivers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Additional signatories, used to collect authorization (on receiving side)\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"allocation\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"approval\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Approval <type-daml-finance-interface-settlement-v4-types-approval-77821>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"batchId\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"consenters\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instructor\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"routedStep\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlementTime\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"settlers\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"signedReceivers\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"signedSenders\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"allocation\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"approval\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Approval <type-daml-finance-interface-settlement-v4-types-approval-77821>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"batchId\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"consenters\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instructor\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"routedStep\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlementTime\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"settlers\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"signedReceivers\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"signedSenders\" `View <type-daml-finance-interface-settlement-v4-instruction-view-97904_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

Functions
---------

.. _function-daml-finance-interface-settlement-v4-instruction-allocate-68742:

`allocate <function-daml-finance-interface-settlement-v4-instruction-allocate-68742_>`_
  \: `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_ \-\> `Allocate <type-daml-finance-interface-settlement-v4-instruction-allocate-48530_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_, `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding))

.. _function-daml-finance-interface-settlement-v4-instruction-approve-7151:

`approve <function-daml-finance-interface-settlement-v4-instruction-approve-7151_>`_
  \: `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_ \-\> `Approve <type-daml-finance-interface-settlement-v4-instruction-approve-69723_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_)

.. _function-daml-finance-interface-settlement-v4-instruction-execute-65941:

`execute <function-daml-finance-interface-settlement-v4-instruction-execute-65941_>`_
  \: `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_ \-\> `Execute <type-daml-finance-interface-settlement-v4-instruction-execute-24017_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding))

.. _function-daml-finance-interface-settlement-v4-instruction-cancel-72271:

`cancel <function-daml-finance-interface-settlement-v4-instruction-cancel-72271_>`_
  \: `Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168_>`_ \-\> `Cancel <type-daml-finance-interface-settlement-v4-instruction-cancel-2291_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ Holding))
