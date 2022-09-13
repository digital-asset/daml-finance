.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Core Interfaces
###############

- ``Daml.Finance.Interface.Holding``

    This package contains the *interface* and utility functions for holdings and accounts. It has the following modules:

    - :ref:`Factory.Account <module-daml-finance-interface-holding-factory-account-66430>`: Interface that allows implementing templates to create accounts.
    - :ref:`Factory.Holding <module-daml-finance-interface-holding-factory-holding-2450>`: Holding factory contract used to create (credit) and archive (debit) holdings.
    - :ref:`Account <module-daml-finance-interface-holding-account-93234>`: Interface which represents an established relationship between a provider and an owner.
    - :ref:`Base <module-daml-finance-interface-holding-base-24195>`: Base interface for a holding.
    - :ref:`Fungible <module-daml-finance-interface-holding-fungible-63712>`: Interface for a fungible holding, which allows splitting and merging.
    - :ref:`Lockable <module-daml-finance-interface-holding-lockable-23737>`: An interface respresenting contracts which allow a set of parties to restrict certain actions on a contract.
    - :ref:`Transferable <module-daml-finance-interface-holding-transferable-88121>`: Interface respresenting a contract where ownership can be transferred to other parties.
    - :ref:`Util <module-daml-finance-interface-holding-util-81618>`: Utility functions related to holdings, e.g. getting the account / instrument / owner of a holding.

    The :doc:`Asset Model <../concepts/asset-model>` page explains the relationship between instruments, holdings, and accounts.
    Check out the :doc:`Transfer tutorial <../../tutorials/getting-started/transfer>` for a description on how to create a holding on an instrument and how to transfer it between accounts.

- ``Daml.Finance.Interface.Settlement``

    This package contains the *interface* for settlement. It has the following modules:

    - :ref:`Instruction <module-daml-finance-interface-settlement-instruction-10970>`: Interface for providing a single instruction to transfer an asset.
    - :ref:`Factory <module-daml-finance-interface-settlement-factory-75196>`: Interface used to generate settlement instructions.
    - :ref:`Batch <module-daml-finance-interface-settlement-batch-39188>`: Interface for atomically settling `Transferable`\s.
    - :ref:`Types <module-daml-finance-interface-settlement-types-44085>`: Types required in the settlement process, e.g. Step, Allocation and Approval.

    The :doc:`Settlement <../concepts/settlement>` page contains an overview of the settlement process and explains the relationship between steps, instructions, and batches.
    Check out the :doc:`Settlement tutorial <../../tutorials/getting-started/settlement>` for a description on how to use settlement workflow in practice.

- ``Daml.Finance.Interface.Lifecycle``

    This package contains the *interface* for lifecycle related processes. It contains the following modules:

    - :ref:`Event <module-daml-finance-interface-lifecycle-event-43586>`: Interface for a lifecycle event, for example that a bond will pay a coupon on a given date.
    - :ref:`Effect <module-daml-finance-interface-lifecycle-effect-16050>`: Interface for contracts exposing effects of lifecycling processes, e.g. the payment resulting from a bond coupon.
    - :ref:`Clock <module-daml-finance-interface-lifecycle-clock-75180>`: Interface for a clock that is used to control time-based events.
    - :ref:`Observable <module-daml-finance-interface-lifecycle-observable-3374>`: Inferface to inspect numerical values (e.g. a stock price or an interest rate) required when processing a lifecycle rule.
    - :ref:`Rule.Claim <module-daml-finance-interface-lifecycle-rule-claim-6739>`: Interface for contracts that allow holders to claim an ``Effect`` and generate settlement instructions.
    - :ref:`Rule.Lifecycle <module-daml-finance-interface-lifecycle-rule-lifecycle-50431>`: Interface implemented by instruments that can be lifecycled.

    The :doc:`Lifecycling <../concepts/lifecycling>` page contains an overview of the lifecycle process and explains the relationship between events, lifecycle rules and effects.
    Check out the :doc:`Lifecycling tutorial <../../tutorials/getting-started/lifecycling>` for a description on how lifecycling works in practice.
    There is also the tutorial :doc:`How to implement a Contingent Claims-based instrument <../../tutorials/instrument-modeling/contingent-claims-instrument>`, which describes how claims are defined, how to use an ``Observable``, and how the ``Lifecycle`` interface is implemented for bonds.

- ``Daml.Finance.Interface.Types``

    This package mainly contains types related to dates and keys. They are defined in the following modules:

    - :ref:`Date.Calendar <module-daml-finance-interface-types-date-calendar-23555>`: Types for holiday calendar data and how to adjust non-business days.
    - :ref:`Date.Classes <module-daml-finance-interface-types-date-classes-73544>`: Type class that specifies what can be converted to UTC time.
    - :ref:`Date.DayCount <module-daml-finance-interface-types-date-daycount-90980>`: Type to specify the conventions used to calculate day count fractions.
    - :ref:`Date.RollConvention <module-daml-finance-interface-types-date-rollconvention-76363>`: Types to define date periods and how to roll dates.
    - :ref:`Date.Schedule <module-daml-finance-interface-types-date-schedule-61944>`: Types to define a periodic schedule with a specified frequency, including how to specify stub periods.
    - :ref:`Common <module-daml-finance-interface-types-common-24625>`: Various types related to keys, observers, parties, identifiers and quantities, which are commonly used in several packages.

- ``Daml.Finance.Interface.Util``

    This package contains the *interface* for the disclosure of contracts and some commonly used utility functions. They are defined in these modules:

    - :ref:`Disclosure <module-daml-finance-interface-util-disclosure-87755>`: An interface for managing the visibility of contracts for non-authorizing parties.
    - :ref:`Common <module-daml-finance-interface-util-common-43703>`: Different utility functions related to interfaces and assertions.
