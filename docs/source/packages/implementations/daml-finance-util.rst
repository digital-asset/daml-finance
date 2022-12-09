.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Util
#################

This package mainly contains utility functions related to dates, lists, maps, and disclosure. They
are defined in the following modules:

- :ref:`Date.Calendar <module-daml-finance-util-date-calendar-17588>`:
  Functions regarding dates and holiday calendars (business vs non-business days)
- :ref:`Date.DayCount <module-daml-finance-util-date-daycount-38239>`:
  Functions to calculate day count fractions according to different conventions
- :ref:`Date.RollConvention <module-daml-finance-util-date-rollconvention-88672>`:
  Functions to calculate date periods including rolling dates
- :ref:`Date.Schedule <module-daml-finance-util-date-schedule-32303>`:
  Functions to calculate a periodic schedule, including both adjusted and unadjusted dates
- :ref:`Common <module-daml-finance-util-common-41560>`:
  Various functions related to lists and maps, which are commonly used in several packages
- :ref:`Disclosure <module-daml-finance-util-disclosure-73352>`:
  Utility functions related to disclosure, e.g., to add or remove observers
