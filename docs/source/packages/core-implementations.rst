.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Core Implementations
####################

- ``Daml.Finance.Instrument.Base``

    This package contains the *implementation* of a basic instrument. It contains the following module:

    - :ref:`Instrument <module-daml-finance-instrument-base-instrument-53549>`: Base implementation of an Instrument which does not define any lifecycling logic.

    Check out the :doc:`Transfer tutorial <../../tutorial/getting-started/transfer>` for an example on how to create a base instrument and use it for a transfer.

- ``Daml.Finance.Instrument.Bond``

    This package contains the *implementation* of different bond types, defined in the following modules:

    - :ref:`FixedRate <module-daml-finance-instrument-bond-fixedrate-44039>`: This template models a fixed rate bond. It pays a fixed coupon rate at the end of every coupon period.
    - :ref:`FloatingRate <module-daml-finance-instrument-bond-floatingrate-31782>`: This template models a floating rate bond. It pays a floating coupon rate at the end of every coupon period.
    - :ref:`InflationLinked <module-daml-finance-instrument-bond-inflationlinked-38254>`: This template models an inflation linked bond. It pays an inflation adjusted coupon at the end of every coupon period.
    - :ref:`ZeroCoupon <module-daml-finance-instrument-bond-zerocoupon-72656>`: This template models a zero coupon bond. It does not pay any coupons, only the redemption amount at maturity.
    - :ref:`Util <module-daml-finance-instrument-bond-util-70458>`: Utility functions related to creating Contingent Claims for coupons / redemption and bond lifecycling logic.

    Check out the tutorial on :doc:`How to use the Bond extension package <../../tutorial/instrument-modelling/bond-extension>` for a description how to use the bond extension in practice.
    There is also the tutorial :doc:`How to implement a Contingent Claims-based instrument <../../tutorial/instrument-modelling/contingent-claims-instrument>`, which describes how the claims are defined and how the lifecycle interface is implemented for bonds.

- ``Daml.Finance.Instrument.Equity``

    This package contains the *implementation* for equities. It has the following modules:

    - :ref:`Factory <module-daml-finance-instrument-equity-factory-96899>`: Factory template for instrument creation.
    - :ref:`Instrument <module-daml-finance-instrument-equity-instrument-69265>`: Instrument representing a common stock.

    For a detailed explanation of the equity extension, check out the ``src/test/daml/Daml/Finance/Instrument/Equity/Test`` folder. It demonstrates how to originate an equity instrument,
    how to create and lifecycle a cash dividend, and how to handle corporate actions like mergers and stock splits.

- ``Daml.Finance.Instrument.Generic``

    This package contains the *implementation* and types required for generic instruments, including lifecycling logic. It contains the following modules:

    - :ref:`Election <module-daml-finance-instrument-generic-election-56972>`: Implementation of Election (e.g. the exercise of an option) and ElectionFactory (to delegate the right to create Elections).
    - :ref:`Factory <module-daml-finance-instrument-generic-factory-42712>`: Factory template for generic instrument creation.
    - :ref:`Instrument <module-daml-finance-instrument-generic-instrument-67364>`: An instrument representing a generic payoff, modelled using ``Contingent Claims``.

    The tutorial :doc:`How to use the Derivative extension to model generic instruments <../../tutorial/instrument-modelling/derivative-extension>` describes how a payoff is defined using ``Contingent Claims`` in practice.

- ``Daml.Finance.Holding``

    This package contains the *implementation* and utility functions for holdings and accounts. It has the following modules:

    - :ref:`Account <module-daml-finance-holding-account-88149>`: A relationship between a custodian and an asset owner. It is referenced by holdings.
    - :ref:`Fungible <module-daml-finance-holding-fungible-7201>`: Implementation of a fungible holding, including split and merge functionality.
    - :ref:`NonFungible <module-daml-finance-holding-nonfungible-86571>`: Implementation of a non-fungible holding, which cannot be split or merged.
    - :ref:`NonTransferable <module-daml-finance-holding-nontransferable-44402>`: Implementation of a non-transferable holding.
    - :ref:`Util <module-daml-finance-holding-util-87323>`: Utility functions related to holdings, e.g. locking a holding.

    The :doc:`Core Concepts <../core-concepts>` page explains the relationship between instruments, holdings and accounts.
    Check out the :doc:`Transfer tutorial <../../tutorial/getting-started/transfer>` for a description on how to create a holding on an instrument and how to transfer it between accounts.

- Daml.Finance.Settlement
    - Batch
    - Instruction
    - Factory
- ``Daml.Finance.Lifecycle``

    This package contains the *implementation* of lifecycle related processes. It contains the following modules:

    - :ref:`Effect <module-daml-finance-lifecycle-effect-1975>`: A contract encoding the *consequences of a lifecycle event* for one unit of the target instrument.
    - :ref:`ElectionEffect <module-daml-finance-lifecycle-electioneffect-99924>`: A contract encoding the *consequences of an election* for one unit of the target instrument.
    - :ref:`Rule.Claim <module-daml-finance-lifecycle-rule-claim-99318>`: Rule contract that allows an actor to process/claim effects, returning settlement instructions.
    - :ref:`Rule.Distribution <module-daml-finance-lifecycle-rule-distribution-35531>`: Rule contract that defines the distribution of units of an instrument for each unit of a target instrument (e.g. share or cash dividends).
    - :ref:`Rule.Replacement <module-daml-finance-lifecycle-rule-replacement-6984>`: Rule contract that defines the replacement of units of an instrument with a basket of other instruments (e.g. stock merger).
    - :ref:`Event.Distribution <module-daml-finance-lifecycle-event-distribution-17302>`: Event contract for the distribution of units of an instrument for each unit of a target instrument (e.g. share or cash dividends).
    - :ref:`Event.Replacement <module-daml-finance-lifecycle-event-replacement-51859>`: Event contract for the replacement of units of an instrument with a basket of other instruments (e.g. stock merger).

    Check out the :doc:`Lifecycling tutorial <../../tutorial/getting-started/lifecycling>` for a description on how lifecycling works in practice, including how to ``Claim`` an ``Effect``.
    There is also the tutorial :doc:`How to implement a Contingent Claims-based instrument <../../tutorial/instrument-modelling/contingent-claims-instrument>`, which describes how create an ``Effect``.
    For a description of ``Distribution`` and ``Replacement``, check out the ``src/test/daml/Daml/Finance/Instrument/Equity/Test`` folder. It demonstrates
    how to create and lifecycle a cash dividend, and how to handle corporate actions like mergers and stock splits.

- ``Daml.Finance.RefData``

    This package contains the *implementation* of reference data related workflows. It contains the following modules:

    - :ref:`Time.DateClock <module-daml-finance-refdata-time-dateclock-80226>`: Event signalling the update of a clock. This can trigger the execution of lifecycle rules for some instruments.
    - :ref:`Observation <module-daml-finance-refdata-observation-94498>`: An implementation of ``Observable`` that explicitly stores time-dependent numerical values (e.g. equity or rate fixings).
    - :ref:`HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891>`: Holiday calendar of an entity (typically an exchange or a currency).

- ``Daml.Finance.Util``

    This package mainly contains functions related to dates, lists and maps. They are defined in the following modules:

    - :ref:`Date.Calendar <module-daml-finance-util-date-calendar-17588>`: Functions regarding dates and holiday calendars (business vs non-business days).
    - :ref:`Date.DayCount <module-daml-finance-util-date-daycount-38239>`: Functions to calculate day count fractions according to different conventions.
    - :ref:`Date.RollConvention <module-daml-finance-util-date-rollconvention-88672>`: Functions to calculate date periods including rolling dates.
    - :ref:`Date.Schedule <module-daml-finance-util-date-schedule-32303>`: Functions to calculate a periodic schedule, including both adjusted and unadjusted dates.
    - :ref:`Common <module-daml-finance-util-common-41560>`: Various functions related to lists and maps, which are commonly used in several packages.
