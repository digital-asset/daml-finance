.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Types.Date - Changelog
#############################################

Version 2.0.0
*************

- Introduce `ScheduleFrequency` data type

- Added `NoRollConvention` to `RollConventionEnum`. It applies to `D` and `W` periods

- `PeriodicSchedule.frequency` is of type `ScheduleFrequency` instead of `Frequency`
