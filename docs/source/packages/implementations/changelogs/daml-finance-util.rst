.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Util.V4
====================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

- Only release a Semaphore lock if the provided context is recognized.

Daml.Finance.Util
=================

Version 3.1.0
*************

- Update of SDK version and dependencies.

- Added a `Lockable` module containing the `aquireImpl` and `releaseImpl` locking utitlity
  functions.

- Fix a bug in the schedule roll-out logic.

- Added new day-count conventions: Act365NL, Basis30365, and Basis30E2360.

Version 3.0.0
*************

- Update of SDK version and dependencies.

- Remove the `groupBy` utility function.

Version 2.0.0
*************

- `calcPeriodDcf` and `calcPeriodDcfActActISMA` take a `ScheduleFrequency` instead of a `Frequency`.

- Dependencies update.
