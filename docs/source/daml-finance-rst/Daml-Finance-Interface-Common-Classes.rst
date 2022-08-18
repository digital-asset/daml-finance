.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-common-classes-96420:

Module Daml.Finance.Interface.Common.Classes
============================================

Typeclasses
-----------

.. _class-daml-finance-interface-common-classes-hasutctimeconversion-72400:

**class** `HasUTCTimeConversion <class-daml-finance-interface-common-classes-hasutctimeconversion-72400_>`_ a **where**

  Types that admit a conversion to an UTC time\.
  
  .. _function-daml-finance-interface-common-classes-toutctime-24021:
  
  `toUTCTime <function-daml-finance-interface-common-classes-toutctime-24021_>`_
    \: a \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
  
  **instance** `HasUTCTimeConversion <class-daml-finance-interface-common-classes-hasutctimeconversion-72400_>`_ :ref:`Clock <type-daml-finance-interface-lifecycle-clock-clock-52275>`
  
  **instance** `HasUTCTimeConversion <class-daml-finance-interface-common-classes-hasutctimeconversion-72400_>`_ :ref:`DateClock <type-daml-finance-refdata-time-dateclock-dateclock-68517>`
  
  **instance** `HasUTCTimeConversion <class-daml-finance-interface-common-classes-hasutctimeconversion-72400_>`_ :ref:`Unit <type-daml-finance-refdata-time-dateclock-unit-39282>`
