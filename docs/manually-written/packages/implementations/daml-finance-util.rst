.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Util.V4
####################

This package primarily contains utility functions related to dates (see the
:doc:`interface docs <../interfaces/daml-finance-interface-types-date>` for an introduction), lists,
maps, and disclosure. They are defined in the following modules:

- :ref:`Date.Calendar <module-daml-finance-util-v4-date-calendar-71711>`:
  Functions regarding dates and holiday calendars (business vs non-business days)
- :ref:`Date.DayCount <module-daml-finance-util-v4-date-daycount-38488>`:
  Functions to calculate day count fractions according to different conventions
- :ref:`Date.RollConvention <module-daml-finance-util-v4-date-rollconvention-61455>`:
  Functions to calculate date periods including rolling dates
- :ref:`Date.Schedule <module-daml-finance-util-v4-date-schedule-63724>`:
  Functions to calculate a periodic schedule, including both adjusted and unadjusted dates
- :ref:`Common <module-daml-finance-util-v4-common-85309>`:
  Various functions related to lists and maps, which are used in several packages
- :ref:`Disclosure <module-daml-finance-util-v4-disclosure-31001>`:
  Utility functions related to disclosure, e.g., to add or remove observers
- :ref:`Lockable <module-daml-finance-util-v4-lockable-69357>`:
  Default implementation for the locking of holdings (acquire, release and validation check).

Changelog
*********
