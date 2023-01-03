.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Data
###########################

This package contains the *interface* for inspecting and working with observables, which are used
in the context of lifecycling. It contains the following modules:

- :ref:`Numeric.Observation.Factory <module-daml-finance-interface-data-numeric-observation-factory-57560>`:
  Interface for a factory used to create, remove and view a ``Numeric.Observation``
- :ref:`Numeric.Observation <module-daml-finance-interface-data-numeric-observation-99152>`:
  Interface for a time-dependent ``Numeric.Observation``, where the values are explicitly stored on-ledger
- :ref:`Reference.HolidayCalendar.Factory <module-daml-finance-interface-data-reference-holidaycalendar-factory-22148>`:
  Interface for a factory used to create, remove and view a ``HolidayCalendar``
- :ref:`Reference.HolidayCalendar <module-daml-finance-interface-data-reference-holidaycalendar-19648>`:
  Interface for contracts storing holiday calendar data on the ledger
- :ref:`Reference.Time <module-daml-finance-interface-data-reference-time-54882>`:
  Interface for contracts that control business time, providing choices to advance or rewind time
