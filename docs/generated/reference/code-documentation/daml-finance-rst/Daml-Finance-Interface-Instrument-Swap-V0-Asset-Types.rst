.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-asset-types-5044:

Daml.Finance.Interface.Instrument.Swap.V0.Asset.Types
=====================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409:

**data** `Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409_>`_

  Describes the attributes of an Asset swap\.

  .. _constr-daml-finance-interface-instrument-swap-v0-asset-types-asset-98554:

  `Asset <constr-daml-finance-interface-instrument-swap-v0-asset-types-asset-98554_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument's key\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The description of the swap\.
       * - underlyings
         - \[`Underlying <type-daml-finance-interface-instrument-swap-v0-asset-types-underlying-93813_>`_\]
         - The list of underlyings (the basket of reference assets for the asset swap)\.
       * - ownerReceivesRate
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - Indicate whether a holding owner of this instrument receives the rate or the asset leg of the swap\.
       * - floatingRate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149>`
         - A description of the floating rate to be used (if applicable)\. This supports both Libor and SOFR style reference rates (using a compounded index, e\.g\. the SOFR Index)\.
       * - fixRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The interest rate of the fix leg\. For example, in case of \"AAPL total return vs 2\.5% fix\" this should be 0\.025\. This can also be used as a floating rate spread\. For example, in case of \"3M Libor \+ 0\.5%\" this should be 0\.005\.
       * - periodicSchedule
         - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
         - The schedule for the periodic swap payments\.
       * - holidayCalendarIds
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         - The identifiers of the holiday calendars to be used for the swap payment schedule\.
       * - calendarDataProvider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The reference data provider to use for the holiday calendar\.
       * - dayCountConvention
         - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
         - The day count convention used to calculate day count fractions\. For example\: Act360\.
       * - currency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The currency of the swap\. For example, if the swap pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409_>`_

.. _type-daml-finance-interface-instrument-swap-v0-asset-types-underlying-93813:

**data** `Underlying <type-daml-finance-interface-instrument-swap-v0-asset-types-underlying-93813_>`_

  Describes an underlying of a product\.

  .. _constr-daml-finance-interface-instrument-swap-v0-asset-types-underlying-22152:

  `Underlying <constr-daml-finance-interface-instrument-swap-v0-asset-types-underlying-22152_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - referenceAsset
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The underlying's key\.
       * - referenceAssetId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The reference asset ID\. This is used to retrieve observations for this underlying\.
       * - weight
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The basket weight of the underlying\.
       * - initialPrice
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The initial price of the underlying\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Underlying <type-daml-finance-interface-instrument-swap-v0-asset-types-underlying-93813_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Underlying <type-daml-finance-interface-instrument-swap-v0-asset-types-underlying-93813_>`_
