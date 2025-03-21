.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Data.V4
####################

This package implements templates containing reference data. It includes the following modules:

- :ref:`Numeric.Observation <module-daml-finance-data-v4-numeric-observation-19522>`:
  An implementation of an observation that explicitly stores time-dependent numerical values on the
  ledger. It can be used to, e.g., store equity or rate fixings.
- :ref:`Reference.HolidayCalendar <module-daml-finance-data-v4-reference-holidaycalendar-15110>`:
  A holiday calendar of an entity (typically an exchange or a currency)
- :ref:`Time.DateClock.Types <module-daml-finance-data-v4-time-dateclock-types-32520>`:
  A date type which can be converted to time, and time-related utility functions
- :ref:`Time.DateClock <module-daml-finance-data-v4-time-dateclock-1389>`:
  A contract specifying what is the current local date. It is used to inject date information in
  lifecycle processing rules
- :ref:`Time.DateClockUpdate <module-daml-finance-data-v4-time-dateclockupdate-59678>`:
  A contract representing passing of (market) time that can be used to trigger contractual,
  time-based `effects <#lifecycling-effect>`__, like interest payments on a bond. It is, for
  example, used to drive the evolution and lifecycling of
  :doc:`Contingent Claims <../../instruments/generic/contingent-claims>`-based instruments.
- :ref:`Time.LedgerTime <module-daml-finance-data-v4-time-ledgertime-80144>`:
  A time observable which uses ledger time

Changelog
*********
