.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-claims-v3-util-date-8229:

Daml.Finance.Claims.V3.Util.Date
================================

Data Types
----------

.. _type-daml-finance-claims-v3-util-date-o-78065:

**type** `O <type-daml-finance-claims-v3-util-date-o-78065_>`_
  \= :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>`

Functions
---------

.. _function-daml-finance-claims-v3-util-date-convertimplicitdcftoactualdcf-21923:

`convertImplicitDcfToActualDcf <function-daml-finance-claims-v3-util-date-convertimplicitdcftoactualdcf-21923_>`_
  \: `O <type-daml-finance-claims-v3-util-date-o-78065_>`_ \-\> :ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> `O <type-daml-finance-claims-v3-util-date-o-78065_>`_

  Calculate a conversion factor if the dcf used for a floating rate compounded index does not
  match the dcf used for an instrument\.
