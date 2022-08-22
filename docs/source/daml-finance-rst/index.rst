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
   Daml.Finance.Asset.Instrument <Daml-Finance-Asset-Instrument>
   Daml.Finance.Asset.NonFungible <Daml-Finance-Asset-NonFungible>
   Daml.Finance.Asset.NonTransferable <Daml-Finance-Asset-NonTransferable>
   Daml.Finance.Bond.FixedRate <Daml-Finance-Bond-FixedRate>
   Daml.Finance.Bond.FloatingRate <Daml-Finance-Bond-FloatingRate>
   Daml.Finance.Bond.InflationLinked <Daml-Finance-Bond-InflationLinked>
   Daml.Finance.Bond.Util <Daml-Finance-Bond-Util>
   Daml.Finance.Bond.ZeroCoupon <Daml-Finance-Bond-ZeroCoupon>
   Daml.Finance.Common.Date.Calendar <Daml-Finance-Common-Date-Calendar>
   Daml.Finance.Common.Date.DayCount <Daml-Finance-Common-Date-DayCount>
   Daml.Finance.Common.Date.RollConvention <Daml-Finance-Common-Date-RollConvention>
   Daml.Finance.Common.Date.Schedule <Daml-Finance-Common-Date-Schedule>
   Daml.Finance.Common.Util <Daml-Finance-Common-Util>
   Daml.Finance.Generic.Election <Daml-Finance-Generic-Election>
   Daml.Finance.Generic.Factory <Daml-Finance-Generic-Factory>
   Daml.Finance.Generic.Instrument <Daml-Finance-Generic-Instrument>
   Daml.Finance.Equity.Factory <Daml-Finance-Equity-Factory>
   Daml.Finance.Equity.Instrument <Daml-Finance-Equity-Instrument>
   Daml.Finance.Interface.Asset.Account <Daml-Finance-Interface-Asset-Account>
   Daml.Finance.Interface.Asset.Factory.Account <Daml-Finance-Interface-Asset-Factory-Account>
   Daml.Finance.Interface.Asset.Factory.Holding <Daml-Finance-Interface-Asset-Factory-Holding>
   Daml.Finance.Interface.Asset.Factory.Instrument <Daml-Finance-Interface-Asset-Factory-Instrument>
   Daml.Finance.Interface.Asset.Fungible <Daml-Finance-Interface-Asset-Fungible>
   Daml.Finance.Interface.Asset.Holding <Daml-Finance-Interface-Asset-Holding>
   Daml.Finance.Interface.Asset.Instrument <Daml-Finance-Interface-Asset-Instrument>
   Daml.Finance.Interface.Asset.Lockable <Daml-Finance-Interface-Asset-Lockable>
   Daml.Finance.Interface.Asset.Transferable <Daml-Finance-Interface-Asset-Transferable>
   Daml.Finance.Interface.Asset.Types <Daml-Finance-Interface-Asset-Types>
   Daml.Finance.Interface.Asset.Util <Daml-Finance-Interface-Asset-Util>
   Daml.Finance.Interface.Bond.FixedRate <Daml-Finance-Interface-Bond-FixedRate>
   Daml.Finance.Interface.Bond.FloatingRate <Daml-Finance-Interface-Bond-FloatingRate>
   Daml.Finance.Interface.Bond.InflationLinked <Daml-Finance-Interface-Bond-InflationLinked>
   Daml.Finance.Interface.Bond.ZeroCoupon <Daml-Finance-Interface-Bond-ZeroCoupon>
   Daml.Finance.Interface.Common.Classes <Daml-Finance-Interface-Common-Classes>
   Daml.Finance.Interface.Common.Disclosure <Daml-Finance-Interface-Common-Disclosure>
   Daml.Finance.Interface.Common.Types <Daml-Finance-Interface-Common-Types>
   Daml.Finance.Interface.Common.Util <Daml-Finance-Interface-Common-Util>
   Daml.Finance.Interface.Generic.Election <Daml-Finance-Interface-Generic-Election>
   Daml.Finance.Interface.Generic.Factory <Daml-Finance-Interface-Generic-Factory>
   Daml.Finance.Interface.Generic.HasClaims <Daml-Finance-Interface-Generic-HasClaims>
   Daml.Finance.Interface.Generic.Types <Daml-Finance-Interface-Generic-Types>
   Daml.Finance.Interface.Generic.Util.Claims <Daml-Finance-Interface-Generic-Util-Claims>
   Daml.Finance.Interface.Generic.Util.Claims.Lifecycle <Daml-Finance-Interface-Generic-Util-Claims-Lifecycle>
   Daml.Finance.Interface.Equity.Factory <Daml-Finance-Interface-Equity-Factory>
   Daml.Finance.Interface.Equity.Instrument <Daml-Finance-Interface-Equity-Instrument>
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

