.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Types.Date.V3
====================================

Version 3.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Types.Date
=================================

Version 2.1.0
*************

- Update of SDK version and dependencies.

- Added new day-count conventions: Act365NL, Basis30365, and Basis30E2360.

Version 2.0.1
*************

- Update of SDK version and dependencies.

Version 2.0.0
*************

- Introduce `ScheduleFrequency` data type.

- Added `NoRollConvention` to `RollConventionEnum`. It applies to `D` and `W` periods.

- `PeriodicSchedule.frequency` is of type `ScheduleFrequency` instead of `Frequency`.
