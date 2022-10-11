.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Core Implementations
####################

- ``Daml.Finance.Holding``

    This package contains the *implementation* and utility functions for holdings. It has the following modules:

    - :ref:`Fungible <module-daml-finance-holding-fungible-7201>`: Implementation of a fungible holding, including split and merge functionality
    - :ref:`NonFungible <module-daml-finance-holding-nonfungible-86571>`: Implementation of a non-fungible holding, which cannot be split or merged
    - :ref:`NonTransferable <module-daml-finance-holding-nontransferable-44402>`: Implementation of a non-transferable holding
    - :ref:`Util <module-daml-finance-holding-util-87323>`: Utility functions related to holdings, e.g. locking a holding

    The :doc:`Asset Model <../concepts/asset-model>` page explains the relationship between instruments, holdings, and accounts.
    Check out the :doc:`Transfer tutorial <../tutorials/getting-started/transfer>` for a description of how to create a holding on an instrument and transfer it between accounts.

- ``Daml.Finance.Account``

    This package contains the *implementation* and utility functions for accounts. It has the following modules:

    - :ref:`Account <module-daml-finance-account-account-19369>`: A relationship between a custodian and an asset owner, referenced by holdings


- ``Daml.Finance.Settlement``

    This package contains the *implementation* of the components used for settlement. It has the following modules:

    - :ref:`Instruction <module-daml-finance-settlement-instruction-87187>`: Used to settle a single settlement `Step`
    - :ref:`Factory <module-daml-finance-settlement-factory-257>`: Used to create a set of settlement `Instruction`\s, and a `Batch` to atomically settle them
    - :ref:`Batch <module-daml-finance-settlement-batch-95573>`: Allows you to atomically settle a set of settlement steps

    The :doc:`Settlement <../concepts/settlement>` page contains an overview of the settlement process and explains the relationship between ``Step``, ``Instruction`` and ``Batch``.
    Check out the :doc:`Settlement tutorial <../tutorials/getting-started/settlement>` for a description on how to implement the settlement workflow in practice.

- ``Daml.Finance.Lifecycle``

    This package contains the *implementation* of lifecycle related processes. It contains the following modules:

    - :ref:`Effect <module-daml-finance-lifecycle-effect-1975>`: A contract encoding the *consequences of a lifecycle event* for one unit of the target instrument
    - :ref:`ElectionEffect <module-daml-finance-lifecycle-electioneffect-99924>`: A contract encoding the *consequences of an election* for one unit of the target instrument
    - :ref:`Rule.Claim <module-daml-finance-lifecycle-rule-claim-99318>`: Rule contract that allows an actor to process/claim effects, returning settlement instructions
    - :ref:`Rule.Distribution <module-daml-finance-lifecycle-rule-distribution-35531>`: Rule contract that defines the distribution of units of an instrument for each unit of a target instrument (e.g. share or cash dividends)
    - :ref:`Rule.Replacement <module-daml-finance-lifecycle-rule-replacement-6984>`: Rule contract that defines the replacement of units of an instrument with a basket of other instruments (e.g. stock merger)
    - :ref:`Event.Distribution <module-daml-finance-lifecycle-event-distribution-17302>`: Event contract for the distribution of units of an instrument for each unit of a target instrument (e.g. share or cash dividends)
    - :ref:`Event.Replacement <module-daml-finance-lifecycle-event-replacement-51859>`: Event contract for the replacement of units of an instrument with a basket of other instruments (e.g. stock merger)
    - :ref:`Event.DateClock <module-daml-finance-lifecycle-event-dateclock-57483>`: Event signalling the update of a clock. This can trigger the execution of lifecycle rules for some instruments

    Check out the :doc:`Lifecycling tutorial <../tutorials/getting-started/lifecycling>` for a description on how lifecycling works in practice, including how to ``Claim`` an ``Effect``.
    There is also the tutorial :doc:`How to implement a Contingent Claims-based instrument <../tutorials/instrument-modeling/contingent-claims-instrument>`, which describes how create an ``Effect``.
    For a description of ``Distribution`` and ``Replacement``, check out the ``src/test/daml/Daml/Finance/Instrument/Equity/Test`` folder. It demonstrates
    how to create and lifecycle a cash dividend, and how to handle corporate actions like mergers and stock splits.

- ``Daml.Finance.Data``

    This package implements templates containing reference data. It includes the following modules:

    - :ref:`Observation <module-daml-finance-data-observable-observation-7524>`: An implementation of an ``Observation`` that explicitly stores time-dependent numerical values on the ledger. It can be used to e.g. store equity or rate fixings
    - :ref:`HolidayCalendar <module-daml-finance-data-reference-holidaycalendar-10773>`: Holiday calendar of an entity (typically an exchange or a currency)
    - :ref:`DateClock <module-daml-finance-data-time-dateclock-65212>`: A contract specifying what is the current local date. It is used to inject date information in lifecycle processing rules

- ``Daml.Finance.Util``

    This package mainly contains functions related to dates, lists and maps. They are defined in the following modules:

    - :ref:`Date.Calendar <module-daml-finance-util-date-calendar-17588>`: Functions regarding dates and holiday calendars (business vs non-business days)
    - :ref:`Date.DayCount <module-daml-finance-util-date-daycount-38239>`: Functions to calculate day count fractions according to different conventions
    - :ref:`Date.RollConvention <module-daml-finance-util-date-rollconvention-88672>`: Functions to calculate date periods including rolling dates
    - :ref:`Date.Schedule <module-daml-finance-util-date-schedule-32303>`: Functions to calculate a periodic schedule, including both adjusted and unadjusted dates
    - :ref:`Common <module-daml-finance-util-common-41560>`: Various functions related to lists and maps, which are commonly used in several packages
