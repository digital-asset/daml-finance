.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Data
#################

This package implements templates containing reference data. It includes the following modules:

- :ref:`Observable.Observation <module-daml-finance-data-observable-observation-7524>`:
  An implementation of an ``Observation`` that explicitly stores time-dependent numerical values
  on the ledger. It can be used to, e.g., store equity or rate fixings
- :ref:`Reference.HolidayCalendar <module-daml-finance-data-reference-holidaycalendar-10773>`:
  Holiday calendar of an entity (typically an exchange or a currency)
- :ref:`Time.DateClock <module-daml-finance-data-time-dateclock-65212>`:
  A contract specifying what is the current local date. It is used to inject date information in
  lifecycle processing rules
- :ref:`Time.DateClock.Types <module-daml-finance-data-time-dateclock-types-48777>`:
  A date type which can be converted to time, and time-related utility funtions

