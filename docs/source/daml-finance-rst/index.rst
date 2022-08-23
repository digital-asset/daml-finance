.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _daml-finance-reference-base:

Daml Finance
============

Here is a complete list of modules in the financial library:

.. toctree::
   :maxdepth: 3
   :titlesonly:
   
   Daml.Finance.Asset.Account <Daml-Finance-Asset-Account>
   Daml.Finance.Asset.Fungible <Daml-Finance-Asset-Fungible>
   Daml.Finance.Asset.NonFungible <Daml-Finance-Asset-NonFungible>
   Daml.Finance.Asset.NonTransferable <Daml-Finance-Asset-NonTransferable>
   Daml.Finance.Common.Date.Calendar <Daml-Finance-Common-Date-Calendar>
   Daml.Finance.Common.Date.DayCount <Daml-Finance-Common-Date-DayCount>
   Daml.Finance.Common.Date.RollConvention <Daml-Finance-Common-Date-RollConvention>
   Daml.Finance.Common.Date.Schedule <Daml-Finance-Common-Date-Schedule>
   Daml.Finance.Common.Util <Daml-Finance-Common-Util>
   Daml.Finance.Instrument.Base.Instrument <Daml-Finance-Instrument-Base-Instrument>
   Daml.Finance.Instrument.Bond.FixedRate <Daml-Finance-Instrument-Bond-FixedRate>
   Daml.Finance.Instrument.Bond.FloatingRate <Daml-Finance-Instrument-Bond-FloatingRate>
   Daml.Finance.Instrument.Bond.InflationLinked <Daml-Finance-Instrument-Bond-InflationLinked>
   Daml.Finance.Instrument.Bond.Util <Daml-Finance-Instrument-Bond-Util>
   Daml.Finance.Instrument.Bond.ZeroCoupon <Daml-Finance-Instrument-Bond-ZeroCoupon>
   Daml.Finance.Instrument.Equity.Factory <Daml-Finance-Instrument-Equity-Factory>
   Daml.Finance.Instrument.Equity.Instrument <Daml-Finance-Instrument-Equity-Instrument>
   Daml.Finance.Instrument.Generic.Election <Daml-Finance-Instrument-Generic-Election>
   Daml.Finance.Instrument.Generic.Factory <Daml-Finance-Instrument-Generic-Factory>
   Daml.Finance.Instrument.Generic.Instrument <Daml-Finance-Instrument-Generic-Instrument>
   Daml.Finance.Interface.Asset.Account <Daml-Finance-Interface-Asset-Account>
   Daml.Finance.Interface.Asset.Factory.Account <Daml-Finance-Interface-Asset-Factory-Account>
   Daml.Finance.Interface.Asset.Factory.Holding <Daml-Finance-Interface-Asset-Factory-Holding>
   Daml.Finance.Interface.Asset.Fungible <Daml-Finance-Interface-Asset-Fungible>
   Daml.Finance.Interface.Asset.Holding <Daml-Finance-Interface-Asset-Holding>
   Daml.Finance.Interface.Asset.Lockable <Daml-Finance-Interface-Asset-Lockable>
   Daml.Finance.Interface.Asset.Transferable <Daml-Finance-Interface-Asset-Transferable>
   Daml.Finance.Interface.Asset.Util <Daml-Finance-Interface-Asset-Util>
   Daml.Finance.Interface.Common.Classes <Daml-Finance-Interface-Common-Classes>
   Daml.Finance.Interface.Common.Disclosure <Daml-Finance-Interface-Common-Disclosure>
   Daml.Finance.Interface.Common.Types <Daml-Finance-Interface-Common-Types>
   Daml.Finance.Interface.Common.Util <Daml-Finance-Interface-Common-Util>
   Daml.Finance.Interface.Instrument.Base.Factory <Daml-Finance-Interface-Instrument-Base-Factory>
   Daml.Finance.Interface.Instrument.Base.Instrument <Daml-Finance-Interface-Instrument-Base-Instrument>
   Daml.Finance.Interface.Instrument.Bond.FixedRate <Daml-Finance-Interface-Instrument-Bond-FixedRate>
   Daml.Finance.Interface.Instrument.Bond.FloatingRate <Daml-Finance-Interface-Instrument-Bond-FloatingRate>
   Daml.Finance.Interface.Instrument.Bond.InflationLinked <Daml-Finance-Interface-Instrument-Bond-InflationLinked>
   Daml.Finance.Interface.Instrument.Bond.ZeroCoupon <Daml-Finance-Interface-Instrument-Bond-ZeroCoupon>
   Daml.Finance.Interface.Instrument.Equity.Factory <Daml-Finance-Interface-Instrument-Equity-Factory>
   Daml.Finance.Interface.Instrument.Equity.Instrument <Daml-Finance-Interface-Instrument-Equity-Instrument>
   Daml.Finance.Interface.Instrument.Generic.Election <Daml-Finance-Interface-Instrument-Generic-Election>
   Daml.Finance.Interface.Instrument.Generic.Factory <Daml-Finance-Interface-Instrument-Generic-Factory>
   Daml.Finance.Interface.Instrument.Generic.HasClaims <Daml-Finance-Interface-Instrument-Generic-HasClaims>
   Daml.Finance.Interface.Instrument.Generic.Types <Daml-Finance-Interface-Instrument-Generic-Types>
   Daml.Finance.Interface.Instrument.Generic.Util.Claims <Daml-Finance-Interface-Instrument-Generic-Util-Claims>
   Daml.Finance.Interface.Instrument.Generic.Util.Claims.Lifecycle <Daml-Finance-Interface-Instrument-Generic-Util-Claims-Lifecycle>
   Daml.Finance.Interface.Lifecycle.Clock <Daml-Finance-Interface-Lifecycle-Clock>
   Daml.Finance.Interface.Lifecycle.Effect <Daml-Finance-Interface-Lifecycle-Effect>
   Daml.Finance.Interface.Lifecycle.Event <Daml-Finance-Interface-Lifecycle-Event>
   Daml.Finance.Interface.Lifecycle.Lifecyclable <Daml-Finance-Interface-Lifecycle-Lifecyclable>
   Daml.Finance.Interface.Lifecycle.Observable <Daml-Finance-Interface-Lifecycle-Observable>
   Daml.Finance.Interface.Lifecycle.SettlementRule <Daml-Finance-Interface-Lifecycle-SettlementRule>
   Daml.Finance.Interface.Settlement.Instructable <Daml-Finance-Interface-Settlement-Instructable>
   Daml.Finance.Interface.Settlement.Instruction <Daml-Finance-Interface-Settlement-Instruction>
   Daml.Finance.Interface.Settlement.Settleable <Daml-Finance-Interface-Settlement-Settleable>
   Daml.Finance.Interface.Settlement.Types <Daml-Finance-Interface-Settlement-Types>
   Daml.Finance.Lifecycle.Effect <Daml-Finance-Lifecycle-Effect>
   Daml.Finance.Lifecycle.ElectionEffect <Daml-Finance-Lifecycle-ElectionEffect>
   Daml.Finance.Lifecycle.Event.Distribution <Daml-Finance-Lifecycle-Event-Distribution>
   Daml.Finance.Lifecycle.Event.Replacement <Daml-Finance-Lifecycle-Event-Replacement>
   Daml.Finance.Lifecycle.Rule.Distribution <Daml-Finance-Lifecycle-Rule-Distribution>
   Daml.Finance.Lifecycle.Rule.Replacement <Daml-Finance-Lifecycle-Rule-Replacement>
   Daml.Finance.Lifecycle.Rule.Settlement <Daml-Finance-Lifecycle-Rule-Settlement>
   Daml.Finance.RefData.HolidayCalendar <Daml-Finance-RefData-HolidayCalendar>
   Daml.Finance.RefData.Observation <Daml-Finance-RefData-Observation>
   Daml.Finance.RefData.Time.DateClock <Daml-Finance-RefData-Time-DateClock>
   Daml.Finance.Settlement.Batch <Daml-Finance-Settlement-Batch>
   Daml.Finance.Settlement.Instruction <Daml-Finance-Settlement-Instruction>

