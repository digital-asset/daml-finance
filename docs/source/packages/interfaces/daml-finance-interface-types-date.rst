.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Types.Date.V3
####################################

Financial instruments, especially those related to interest rates, often depend on periodic
schedules. For example, consider a fixed rate bond with a coupon that should be paid every three
months. Such a regular schedule sometimes results in a payment date falling on a weekend or on
a holiday. Those dates need to be adjusted to business dates, according to the terms of the
instrument.

This package contains types related to dates, in particular how to define regular schedules and how
dates should be adjusted when they fall on non-business days. They are defined in the following
modules:

- :ref:`Calendar <module-daml-finance-interface-types-date-v3-calendar-20021>`:
  Types for holiday calendar data and how to adjust non-business days
- :ref:`Classes <module-daml-finance-interface-types-date-v3-classes-49826>`:
  Type class that specifies what can be converted to UTC time
- :ref:`DateOffset <module-daml-finance-interface-types-date-v3-dateoffset-17578>`:
  Types for date offsets that can be used e.g. to specify a rate fixing date relative to the reset
  date in terms of a business days offset and an associated set of financial business centers.
- :ref:`DayCount <module-daml-finance-interface-types-date-v3-daycount-5046>`:
  Type to specify the conventions used to calculate day count fractions. These are used to define
  the interest accrued during each schedule period, for example for a bond or a swap.
- :ref:`RollConvention <module-daml-finance-interface-types-date-v3-rollconvention-38965>`:
  Types to define date periods and how to roll dates
- :ref:`Schedule <module-daml-finance-interface-types-date-v3-schedule-94670>`:
  Types to define date schedules

Check out the :ref:`Fixed rate bond documentation <fixed-rate-bond-tutorial-section>`
for a description of how to use the these date related types in practice.

Changelog
*********
