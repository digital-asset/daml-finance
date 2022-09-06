.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Core Implementations
####################

- Daml.Finance.Asset
    - Holding (signatory model, implementations)
    - Account
    - Factories
- Daml.Finance.Settlement
    - Batch
    - Instruction
    - Factory
- ``Daml.Finance.Lifecycle``

    This package contains the *implementation* of lifecycle related processes. It contains the following modules:

    - :ref:`Effect <module-daml-finance-lifecycle-effect-1975>`: A contract encoding the *consequences of a lifecycle event* for one unit of the target instrument.
    - :ref:`ElectionEffect <module-daml-finance-lifecycle-electioneffect-99924>`: A contract encoding the *consequences of an election* for one unit of the target instrument.
    - :ref:`Rule.Claim <module-daml-finance-lifecycle-rule-claim-99318>`: Rule contract that allows an actor to claim effects, returning settlement instructions.
    - :ref:`Rule.Distribution <module-daml-finance-lifecycle-rule-distribution-35531>`: Rule contract that defines the distribution of units of an instrument for each unit of a target instrument (e.g. share or cash dividends).
    - :ref:`Rule.Replacement <module-daml-finance-lifecycle-rule-replacement-6984>`: Rule contract that defines the replacement of units of an instrument with a basket of other instruments (e.g. stock merger).
    - :ref:`Event.Distribution <module-daml-finance-lifecycle-event-distribution-17302>`: Event contract for the distribution of units of an instrument for each unit of a target instrument (e.g. share or cash dividends).
    - :ref:`Event.Replacement <module-daml-finance-lifecycle-event-replacement-51859>`: Event contract for the replacement of units of an instrument with a basket of other instruments (e.g. stock merger).

    Check out the :doc:`Lifecycling tutorial <../../tutorial/getting-started/lifecycling>` for a description on how lifecycling works in practice, including how to ``Claim`` an ``Effect``.
    There is also the tutorial :doc:`How to implement a Contingent Claims-based instrument <../../tutorial/instrument-modelling/contingent-claims-instrument>`, which describes how create an ``Effect``.
    For a description of ``Distribution`` and ``Replacement``, check out the ``src/test/daml/Daml/Finance/Instrument/Equity/Test`` folder. It demonstrates
    how to create and lifecycle a cash dividend, and how to handle corporate actions like mergers and stock splits.

- Daml.Finance.RefData
    - DateClock
    - Observation
    - HolidayCalendar
- Daml.Finance.Common
    - Date
    - Util
