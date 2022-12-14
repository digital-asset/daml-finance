.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Data
#################

This package implements templates containing reference data. It includes the following modules:

- :ref:`Numeric.Observation <module-daml-finance-data-numeric-observation-78761>`:
  An implementation of an observation that explicitly stores time-dependent numerical values
  on the ledger. It can be used to, e.g., store equity or rate fixings.
- :ref:`Reference.HolidayCalendar <module-daml-finance-data-reference-holidaycalendar-10773>`:
  A holiday calendar of an entity (typically an exchange or a currency)
- :ref:`Time.DateClock.Types <module-daml-finance-data-time-dateclock-types-48777>`:
  A date type which can be converted to time, and time-related utility funtions
- :ref:`Time.DateClock <module-daml-finance-data-time-dateclock-65212>`:
  A contract specifying what is the current local date. It is used to inject date information in
  lifecycle processing rules
- :ref:`Time.DateClockUpdate <module-daml-finance-data-time-dateclockupdate-48859>`:
  A contract representing passing of (market) time that can be used to trigger contractual,
  time-based cashflows, like interest payments on a bond. It is, for example, used to drive the
  evolution and lifecycling of :doc:`Contingent Claims <../../concepts/contingent-claims>`-based instruments.
- :ref:`Time.LedgerTime <module-daml-finance-data-time-ledgertime-84639>`:
  A time observable which uses ledger time

