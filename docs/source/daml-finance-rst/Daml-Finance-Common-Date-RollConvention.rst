.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-common-date-rollconvention-40427:

Module Daml.Finance.Common.Date.RollConvention
==============================================

Data Types
----------

.. _type-daml-finance-common-date-rollconvention-period-30068:

**data** `Period <type-daml-finance-common-date-rollconvention-period-30068_>`_

  A data type to define periods\.
  
  .. _constr-daml-finance-common-date-rollconvention-period-7241:
  
  `Period <constr-daml-finance-common-date-rollconvention-period-7241_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - period
         - `PeriodEnum <type-daml-finance-common-date-rollconvention-periodenum-40915_>`_
         - A period, e\.g\. a day, week, month or year\.
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         - A period multiplier, e\.g\. 1, 2 or 3 etc\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Period <type-daml-finance-common-date-rollconvention-period-30068_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Period <type-daml-finance-common-date-rollconvention-period-30068_>`_

.. _type-daml-finance-common-date-rollconvention-periodenum-40915:

**data** `PeriodEnum <type-daml-finance-common-date-rollconvention-periodenum-40915_>`_

  An enum type to specify a period, e\.g\. day, week\.
  
  .. _constr-daml-finance-common-date-rollconvention-d-36171:
  
  `D <constr-daml-finance-common-date-rollconvention-d-36171_>`_
  
    Day
  
  .. _constr-daml-finance-common-date-rollconvention-m-99334:
  
  `M <constr-daml-finance-common-date-rollconvention-m-99334_>`_
  
    Month
  
  .. _constr-daml-finance-common-date-rollconvention-w-73264:
  
  `W <constr-daml-finance-common-date-rollconvention-w-73264_>`_
  
    Week
  
  .. _constr-daml-finance-common-date-rollconvention-y-47194:
  
  `Y <constr-daml-finance-common-date-rollconvention-y-47194_>`_
  
    Year
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `PeriodEnum <type-daml-finance-common-date-rollconvention-periodenum-40915_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `PeriodEnum <type-daml-finance-common-date-rollconvention-periodenum-40915_>`_

.. _type-daml-finance-common-date-rollconvention-rollconventionenum-16664:

**data** `RollConventionEnum <type-daml-finance-common-date-rollconvention-rollconventionenum-16664_>`_

  An enum type to specify how to roll dates\.
  
  .. _constr-daml-finance-common-date-rollconvention-eom-99904:
  
  `EOM <constr-daml-finance-common-date-rollconvention-eom-99904_>`_
  
    Rolls on month end\.
  
  .. _constr-daml-finance-common-date-rollconvention-dom-99649:
  
  `DOM <constr-daml-finance-common-date-rollconvention-dom-99649_>`_ `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
  
    Rolls on the corresponding day of the month\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `RollConventionEnum <type-daml-finance-common-date-rollconvention-rollconventionenum-16664_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `RollConventionEnum <type-daml-finance-common-date-rollconvention-rollconventionenum-16664_>`_

Functions
---------

.. _function-daml-finance-common-date-rollconvention-next-30118:

`next <function-daml-finance-common-date-rollconvention-next-30118_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Period <type-daml-finance-common-date-rollconvention-period-30068_>`_ \-\> `RollConventionEnum <type-daml-finance-common-date-rollconvention-rollconventionenum-16664_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Get next periodic (daily ``D`` and weekly ``W`` not supported) date according
  to a given roll convention\.

.. _function-daml-finance-common-date-rollconvention-previous-73510:

`previous <function-daml-finance-common-date-rollconvention-previous-73510_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Period <type-daml-finance-common-date-rollconvention-period-30068_>`_ \-\> `RollConventionEnum <type-daml-finance-common-date-rollconvention-rollconventionenum-16664_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Get previous periodic (daily ``D`` and weekly ``W`` not supported) date according
  to a given roll convention\.

.. _function-daml-finance-common-date-rollconvention-addperiod-69390:

`addPeriod <function-daml-finance-common-date-rollconvention-addperiod-69390_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Period <type-daml-finance-common-date-rollconvention-period-30068_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Add period to given date\.
