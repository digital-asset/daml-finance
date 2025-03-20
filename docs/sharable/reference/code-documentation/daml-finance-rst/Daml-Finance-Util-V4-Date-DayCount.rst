.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-util-v4-date-daycount-38488:

Daml.Finance.Util.V4.Date.DayCount
==================================

Functions
---------

.. _function-daml-finance-util-v4-date-daycount-calcdcf-53229:

`calcDcf <function-daml-finance-util-v4-date-daycount-calcdcf-53229_>`_
  \: :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Calculates the day count fraction given the correponding convention\.
  Currently 30E360 is not supported as we do not want to expose the maturity date of the product
  as an additional parameter\.

.. _function-daml-finance-util-v4-date-daycount-calcperioddcf-52338:

`calcPeriodDcf <function-daml-finance-util-v4-date-daycount-calcperioddcf-52338_>`_
  \: :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> :ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`ScheduleFrequency <type-daml-finance-interface-types-date-v3-schedule-schedulefrequency-11056>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Calculate day count fraction for a schedule period\.
  It takes the following parameters\:

  * DayCountConventionEnum\: to specify which day count convention should be used
  * SchedulePeriod\: the schedule period for which the day count fraction should be calculated
  * Bool\: Whether day count fraction should be calculated on adjusted dates
    (if False\: unadjusted dates)
  * Date\: The maturity date of the instrument
  * Frequency\: the frequency of the schedule period

.. _function-daml-finance-util-v4-date-daycount-calcperioddcfactactisda-35249:

`calcPeriodDcfActActIsda <function-daml-finance-util-v4-date-daycount-calcperioddcfactactisda-35249_>`_
  \: :ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Calculate Actual Actual day count fraction according to the ISDA method\.

.. _function-daml-finance-util-v4-date-daycount-calcperioddcfactactisma-69762:

`calcPeriodDcfActActIsma <function-daml-finance-util-v4-date-daycount-calcperioddcfactactisma-69762_>`_
  \: :ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`ScheduleFrequency <type-daml-finance-interface-types-date-v3-schedule-schedulefrequency-11056>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Calculate Actual Actual day count fraction according to the ISMA method\.

.. _function-daml-finance-util-v4-date-daycount-calcdcfactactafb-61269:

`calcDcfActActAfb <function-daml-finance-util-v4-date-daycount-calcdcfactactafb-61269_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _function-daml-finance-util-v4-date-daycount-calcdcfact360-26626:

`calcDcfAct360 <function-daml-finance-util-v4-date-daycount-calcdcfact360-26626_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _function-daml-finance-util-v4-date-daycount-calcdcfact365fixed-8788:

`calcDcfAct365Fixed <function-daml-finance-util-v4-date-daycount-calcdcfact365fixed-8788_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _function-daml-finance-util-v4-date-daycount-calcdcfact365l-68402:

`calcDcfAct365L <function-daml-finance-util-v4-date-daycount-calcdcfact365l-68402_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _function-daml-finance-util-v4-date-daycount-calcdcf30360-60494:

`calcDcf30360 <function-daml-finance-util-v4-date-daycount-calcdcf30360-60494_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _function-daml-finance-util-v4-date-daycount-calcdcf30360icma-79284:

`calcDcf30360Icma <function-daml-finance-util-v4-date-daycount-calcdcf30360icma-79284_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _function-daml-finance-util-v4-date-daycount-calcdcf30e360-448:

`calcDcf30E360 <function-daml-finance-util-v4-date-daycount-calcdcf30e360-448_>`_
  \: `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Calculate 30E/360 day count fraction\.
