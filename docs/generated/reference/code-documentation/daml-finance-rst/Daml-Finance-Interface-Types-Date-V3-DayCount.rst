.. Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-types-date-v3-daycount-5046:

Daml.Finance.Interface.Types.Date.V3.DayCount
=============================================

Data Types
----------

.. _type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31:

**data** `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  An enum type to specify a day count convention used to calculate day count fractions\.
  For a detailed definition of each convention, we refer to the \"Method of Interest Computation
  Indicator\" definitions in the context of the ISO\-20022 standard\. Where useful, we provide
  disambiguation comments\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-act360-36898:

  `Act360 <constr-daml-finance-interface-types-date-v3-daycount-act360-36898_>`_

    Actual 360\.
    In CDM it is called *DayCountFractionEnum\_ACT\_360*\. In ISO20022 it is called *A004*\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-act365fixed-40608:

  `Act365Fixed <constr-daml-finance-interface-types-date-v3-daycount-act365fixed-40608_>`_

    Actual 365 fixed\.
    In CDM it is called *DayCountFractionEnum\_ACT\_365\_FIXED*\. In ISO20022 it is called *A005*\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-act365nl-19647:

  `Act365NL <constr-daml-finance-interface-types-date-v3-daycount-act365nl-19647_>`_

    Actual 365NL (No Leap)\.
    It excludes any leap days from the day count in each period (exclusive\-inclusive)\.
    In ISO20022 it is called *A014*\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-act365l-87322:

  `Act365L <constr-daml-finance-interface-types-date-v3-daycount-act365l-87322_>`_

    Actual 365L\.
    In CDM it is called *DayCountFractionEnum\_ACT\_365L*\. In ISO20022 it is called *A009*\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-actactafb-26905:

  `ActActAFB <constr-daml-finance-interface-types-date-v3-daycount-actactafb-26905_>`_

    Actual Actual AFB\.
    In CDM it is called *DayCountFractionEnum\_ACT\_ACT\_AFB*\. In ISO20022 it is called *A010*\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-actactisda-70394:

  `ActActISDA <constr-daml-finance-interface-types-date-v3-daycount-actactisda-70394_>`_

    Actual Actual ISDA\.
    In CDM it is called *DayCountFractionEnum\_ACT\_ACT\_ISDA*\. In ISO20022 it is called *A008*\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-actacticma-2729:

  `ActActICMA <constr-daml-finance-interface-types-date-v3-daycount-actacticma-2729_>`_

    Actual Actual ICMA\.
    In CDM it is called *DayCountFractionEnum\_ACT\_ACT\_ICMA* and
    *DayCountFractionEnum\_ACT\_ACT\_ISMA* (they are identical\:
    https\://www\.isda\.org/2011/01/07/act\-act\-icma/)\.
    In ISO20022 it is called *A006*\. Also called ISMA in the 1998 ISDA paper\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-basis1-23072:

  `Basis1 <constr-daml-finance-interface-types-date-v3-daycount-basis1-23072_>`_

    1/1\.
    In CDM it is called *DayCountFractionEnum\_\_1\_1*\. Currently not included in ISO20022\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-basis30360-60007:

  `Basis30360 <constr-daml-finance-interface-types-date-v3-daycount-basis30360-60007_>`_

    30/360\.
    In CDM it is called *DayCountFractionEnum\_\_30\_360*\. In ISO20022 it is called *A001*\.
    Also called 30/360 ISDA or American Basic rule\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-basis30365-49750:

  `Basis30365 <constr-daml-finance-interface-types-date-v3-daycount-basis30365-49750_>`_

    30/365\.
    In ISO20022 it is called *A002*\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-basis30360icma-86997:

  `Basis30360ICMA <constr-daml-finance-interface-types-date-v3-daycount-basis30360icma-86997_>`_

    30/360 ICMA\.
    In CDM it is called *DayCountFractionEnum\_\_30E\_360*\. In ISO20022 it is called *A011*\.
    Also called Basic Rule\. This corresponds to \"30E/360\" of the 2006 ISDA definitions\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-basis30e360-6411:

  `Basis30E360 <constr-daml-finance-interface-types-date-v3-daycount-basis30e360-6411_>`_

    30E/360\.
    In CDM it is called *DayCountFractionEnum\_\_30E\_360\_ISDA*\. In ISO20022 it is called *A007*\.
    Also called Eurobond basis\. This corresponds to \"30E360 (ISDA)\" of the 2006 ISDA definitions\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-basis30e2360-88658:

  `Basis30E2360 <constr-daml-finance-interface-types-date-v3-daycount-basis30e2360-88658_>`_

    30E2/360\.
    In ISO20022 it is called *A012*\. Also called Eurobond basis model 2\.

  .. _constr-daml-finance-interface-types-date-v3-daycount-basis30e3360-81973:

  `Basis30E3360 <constr-daml-finance-interface-types-date-v3-daycount-basis30e3360-81973_>`_

    30E3/360\.
    Currently not included in CDM\. In ISO20022 it is called *A013*\. Also called Eurobond basis
    model 3\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Callable <type-daml-finance-interface-instrument-bond-v3-callable-types-callable-12794>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`FixedRate <type-daml-finance-interface-instrument-bond-v3-fixedrate-types-fixedrate-8592>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`FloatingRate <type-daml-finance-interface-instrument-bond-v3-floatingrate-types-floatingrate-91442>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" :ref:`InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountFraction\" :ref:`Calculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-57533>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-fixedrate-instrument-instrument-67562>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-floatingrate-instrument-instrument-91965>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-autocallable-instrument-instrument-72027>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-currency-instrument-instrument-45179>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Instrument <type-daml-finance-instrument-swap-v0-interestrate-instrument-instrument-3842>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Callable <type-daml-finance-interface-instrument-bond-v3-callable-types-callable-12794>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`FixedRate <type-daml-finance-interface-instrument-bond-v3-fixedrate-types-fixedrate-8592>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`FloatingRate <type-daml-finance-interface-instrument-bond-v3-floatingrate-types-floatingrate-91442>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" :ref:`InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountFraction\" :ref:`Calculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-57533>` `DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31_>`_
