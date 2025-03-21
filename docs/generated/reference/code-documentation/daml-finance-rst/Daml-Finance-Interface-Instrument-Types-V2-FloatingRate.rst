.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-types-v2-floatingrate-85519:

Daml.Finance.Interface.Instrument.Types.V2.FloatingRate
=======================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-types-v2-floatingrate-daterelativetoenum-35995:

**data** `DateRelativeToEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-daterelativetoenum-35995_>`_

  The specification of whether payments/resets occur relative to the first or last day of a
  calculation period\.

  .. _constr-daml-finance-interface-instrument-types-v2-floatingrate-calculationperiodstartdate-2130:

  `CalculationPeriodStartDate <constr-daml-finance-interface-instrument-types-v2-floatingrate-calculationperiodstartdate-2130_>`_

    Payments/Resets will occur relative to the first day of each calculation period\.

  .. _constr-daml-finance-interface-instrument-types-v2-floatingrate-calculationperiodenddate-90327:

  `CalculationPeriodEndDate <constr-daml-finance-interface-instrument-types-v2-floatingrate-calculationperiodenddate-90327_>`_

    Payments/Resets will occur relative to the last day of each calculation period\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `DateRelativeToEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-daterelativetoenum-35995_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `DateRelativeToEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-daterelativetoenum-35995_>`_

.. _type-daml-finance-interface-instrument-types-v2-floatingrate-fixingdates-58708:

**type** `FixingDates <type-daml-finance-interface-instrument-types-v2-floatingrate-fixingdates-58708_>`_
  \= :ref:`DateOffset <type-daml-finance-interface-types-date-v3-dateoffset-dateoffset-75159>`

.. _type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149:

**data** `FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149_>`_

  Specifies the data required for a floating rate coupon\.

  .. _constr-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-99628:

  `FloatingRate <constr-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-99628_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - referenceRateId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The identifier of the reference rate to be used for the coupon, e\.g\. Libor\-3M\.
       * - referenceRateType
         - `ReferenceRateTypeEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-referenceratetypeenum-15522_>`_
         - The type of reference rate, which defines how the reference rate is calcuated\.
       * - fixingDates
         - `FixingDates <type-daml-finance-interface-instrument-types-v2-floatingrate-fixingdates-58708_>`_
         - Specifies the fixing dates as an offset of the calculation date, e\.g\. \-2 business days\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149_>`_

.. _type-daml-finance-interface-instrument-types-v2-floatingrate-referenceratetypeenum-15522:

**data** `ReferenceRateTypeEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-referenceratetypeenum-15522_>`_

  The type of reference rate, which defines how the reference rate is calculated\.

  .. _constr-daml-finance-interface-instrument-types-v2-floatingrate-singlefixing-17539:

  `SingleFixing <constr-daml-finance-interface-instrument-types-v2-floatingrate-singlefixing-17539_>`_ `DateRelativeToEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-daterelativetoenum-35995_>`_

    The reference rate is fixed on one observation date\. This is usually the case for Libor
    and similar reference rates\. A DateRelativeToEnum is required to indicate whether the
    reference rate will reset relative to the first or the last day of the calculation period\.

  .. _constr-daml-finance-interface-instrument-types-v2-floatingrate-compoundedindex-69441:

  `CompoundedIndex <constr-daml-finance-interface-instrument-types-v2-floatingrate-compoundedindex-69441_>`_ :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`

    The reference rate is a regularly (e\.g\. daily) compounded reference rate, e\.g\. compounded
    SOFR, calculated via an index that is continuously compounded since a specified start date\.
    This enables efficient calculation using only the index values at the start and at the end of
    the calculation period\: SOFR\_INDEX\_END / SOFR\_INDEX\_START \- 1, as described here\:
    https\://www\.newyorkfed\.org/markets/reference\-rates/additional\-information\-about\-reference\-rates\#tgcr\_bgcr\_sofr\_calculation\_methodology
    The day count convention used for the index calculation (by the index provider) is also
    required\. For example, in the case of SOFR this is Act360, which is implied by the 360/dc
    factor in the formula in the \"Calculation Methodology for the SOFR Averages and Index\"
    section in the link above\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ReferenceRateTypeEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-referenceratetypeenum-15522_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ReferenceRateTypeEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-referenceratetypeenum-15522_>`_
