.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Core Interfaces
###############

- Daml.Finance.Interface.Asset
    - Holding (purpose, roles, functionality)
    - Account (^ + keying)
    - Instrument (^ + keying)
    - Factories  (^ + visibility)
- Daml.Finance.Interface.Settlement
    - Instruction
    - Settleable (-> rename to Settlement / Batch?)
    - Instructable (-> rename to Factory?)
- ``Daml.Finance.Interface.Lifecycle``

    This package contains the *interface* for lifecycle related processes. It contains the following modules:

    - :ref:`Event <module-daml-finance-interface-lifecycle-event-43586>`: Interface for a lifecycle event, for example that a bond will pay a coupon on a given date.
    - :ref:`Effect <module-daml-finance-interface-lifecycle-effect-16050>`: Interface for contracts exposing effects of lifecycling processes, e.g. the payment resulting from a bond coupon.
    - :ref:`Clock <module-daml-finance-interface-lifecycle-clock-75180>`: Interface for a clock that is used to control time-based events.
    - :ref:`Observable <module-daml-finance-interface-lifecycle-observable-3374>`: Inferface to inspect numerical values (e.g. a stock price or an interest rate) required when processing a lifecycle rule.
    - :ref:`Rule.Claim <module-daml-finance-interface-lifecycle-rule-claim-6739>`: Interface for contracts that allow holders to claim an ``Effect`` and generate settlement instructions.
    - :ref:`Rule.Lifecycle <module-daml-finance-interface-lifecycle-rule-lifecycle-50431>`: Interface implemented by instruments that can be lifecycled.

    Check out the :doc:`Lifecycling tutorial <../../tutorial/getting-started/lifecycling>` for a description on how lifecycling works in practice.
    There is also the tutorial :doc:`How to implement a Contingent Claims-based instrument <../../tutorial/instrument-modelling/contingent-claims-instrument>`, which describes how claims are defined, how to use an ``Observable``, and how the ``Lifecycle`` interface is implemented for bonds.

- Daml.Finance.Interface.Common
    - Disclosure
    - Date
