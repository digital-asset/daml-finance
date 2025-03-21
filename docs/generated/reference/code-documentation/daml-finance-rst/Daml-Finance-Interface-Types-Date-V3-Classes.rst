.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-types-date-v3-classes-49826:

Daml.Finance.Interface.Types.Date.V3.Classes
============================================

Typeclasses
-----------

.. _class-daml-finance-interface-types-date-v3-classes-hasutctimeconversion-43124:

**class** `HasUTCTimeConversion <class-daml-finance-interface-types-date-v3-classes-hasutctimeconversion-43124_>`_ a **where**

  Types that can be converted to UTC time\.

  .. _function-daml-finance-interface-types-date-v3-classes-toutctime-62631:

  `toUTCTime <function-daml-finance-interface-types-date-v3-classes-toutctime-62631_>`_
    \: a \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `HasUTCTimeConversion <class-daml-finance-interface-types-date-v3-classes-hasutctimeconversion-43124_>`_ :ref:`DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944>`

  **instance** `HasUTCTimeConversion <class-daml-finance-interface-types-date-v3-classes-hasutctimeconversion-43124_>`_ :ref:`Unit <type-daml-finance-data-v4-time-dateclock-types-unit-49992>`
