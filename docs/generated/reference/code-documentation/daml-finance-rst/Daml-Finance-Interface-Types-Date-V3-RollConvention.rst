.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-types-date-v3-rollconvention-38965:

Daml.Finance.Interface.Types.Date.V3.RollConvention
===================================================

Data Types
----------

.. _type-daml-finance-interface-types-date-v3-rollconvention-period-94990:

**data** `Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990_>`_

  A data type to define periods\.

  .. _constr-daml-finance-interface-types-date-v3-rollconvention-period-57915:

  `Period <constr-daml-finance-interface-types-date-v3-rollconvention-period-57915_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - period
         - `PeriodEnum <type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289_>`_
         - A period, e\.g\., a day, week, month or year\.
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         - A period multiplier, e\.g\., 1, 2 or 3 etc\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990_>`_

.. _type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289:

**data** `PeriodEnum <type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289_>`_

  An enum type to specify a period, e\.g\., day or week\.

  .. _constr-daml-finance-interface-types-date-v3-rollconvention-d-28261:

  `D <constr-daml-finance-interface-types-date-v3-rollconvention-d-28261_>`_

    Day

  .. _constr-daml-finance-interface-types-date-v3-rollconvention-m-60944:

  `M <constr-daml-finance-interface-types-date-v3-rollconvention-m-60944_>`_

    Month

  .. _constr-daml-finance-interface-types-date-v3-rollconvention-w-87014:

  `W <constr-daml-finance-interface-types-date-v3-rollconvention-w-87014_>`_

    Week

  .. _constr-daml-finance-interface-types-date-v3-rollconvention-y-47844:

  `Y <constr-daml-finance-interface-types-date-v3-rollconvention-y-47844_>`_

    Year

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `PeriodEnum <type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `PeriodEnum <type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289_>`_

.. _type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490:

**data** `RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490_>`_

  An enum type to specify how to roll dates\.

  .. _constr-daml-finance-interface-types-date-v3-rollconvention-eom-7578:

  `EOM <constr-daml-finance-interface-types-date-v3-rollconvention-eom-7578_>`_

    Rolls on month end\.

  .. _constr-daml-finance-interface-types-date-v3-rollconvention-dom-47139:

  `DOM <constr-daml-finance-interface-types-date-v3-rollconvention-dom-47139_>`_ `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_

    Rolls on the corresponding day of the month\.

  .. _constr-daml-finance-interface-types-date-v3-rollconvention-norollconvention-34259:

  `NoRollConvention <constr-daml-finance-interface-types-date-v3-rollconvention-norollconvention-34259_>`_

    No roll convention is specified\. This is for e\.g\. when date roll is not required (``D`` or ``W``
    tenors, single\-period schedules)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490_>`_
