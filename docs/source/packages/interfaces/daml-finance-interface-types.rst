.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Types
############################

This package mainly contains types related to dates and keys. They are defined in the following
modules:

- :ref:`Date.Calendar <module-daml-finance-interface-types-date-calendar-23555>`:
  Types for holiday calendar data and how to adjust non-business days
- :ref:`Date.Classes <module-daml-finance-interface-types-date-classes-73544>`:
  Type class that specifies what can be converted to UTC time
- :ref:`Date.DayCount <module-daml-finance-interface-types-date-daycount-90980>`:
  Type to specify the conventions used to calculate day count fractions
- :ref:`Date.RollConvention <module-daml-finance-interface-types-date-rollconvention-76363>`:
  Types to define date periods and how to roll dates
- :ref:`Date.Schedule <module-daml-finance-interface-types-date-schedule-61944>`:
  Types to define a periodic schedule with a specified frequency, including how to specify stub
  periods
- :ref:`Common <module-daml-finance-interface-types-common-24625>`:
  Various types related to keys, observers, parties, identifiers and quantities, which are
  commonly used in several packages
