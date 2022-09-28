.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Lifecycling
###########

:ref:`Lifecycling <lifecycling>` is the evolution of instruments over their lifetime.
The library provides a standard mechanism for processing instruments accross asset types.

Various types of events result in cash flows and updated instrument definitions,
to reflect past events and cash flows that have already been paid.

Events
******

The ``Event`` interface, which is defined in ``Daml.Finance.Interface.Lifecycle.Event``, is
used to handle different types of events:

Intrinsic
=========

Intrinsic events are contractual cash flows.
The contract terms pre-define exactly what triggers these events, for example:

- A certain date is reached, which results in a coupon payent of a bond. Time-based events are controlled using the ``DateClock`` template (not ledger time).
- The price of a stock reaches a certain level, resulting in a barrier hit. The relevant stock price is defined in an ``Observable`` template.

Extrinsic
=========

Extrinsic events, for example corporate actions and elections, are not pre-defined.
These events are triggered by exercising an external choice.

Instrument Versions
*******************

Consider a bond instrument which pays a fixed coupon once a year. In this case, the
coupon payment is an intrinsic, time-based event.

When the bond is issued, the holder is entitled to all future coupon payments.
On the payment date of the first coupon, the bond is lifecycled by the issuer. This has
the following result:

#. The current coupon is paid to the holder of the bond.
#. The bond is replaced by a new version, which includes only the remaining coupons.

It is important to understand that there are two different instruments: one which includes the current coupon and one which does not.

Effects
*******

When an event is lifecycled, an ``Effect`` is produced. This is defined in ``Daml.Finance.Interface.Lifecycle.Effect``.
The ``Effect`` can be settled in order to produce the relevant cash flows and to create the new instrument version (reflecting the remaining cash flows).

These lifecycle concepts are also explained with example code in the :doc:`Lifecycling tutorial <../tutorials/getting-started/lifecycling>`.
