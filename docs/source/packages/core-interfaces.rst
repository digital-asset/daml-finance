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
- Daml.Finance.Interface.Lifecycle
    - Lifecyclable
    - Event
    - Effect
    - Clock
    - Observable
    - Rules
- Daml.Finance.Interface.Common
    - Disclosure
    - Date
