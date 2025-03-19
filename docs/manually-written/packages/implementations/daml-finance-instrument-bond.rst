.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Instrument.Bond.V3
###############################

This package contains the *implementation* of different bond types, defined in the following
modules:

- :ref:`Callable.Instrument <module-daml-finance-instrument-bond-v3-callable-instrument-35206>`:
  Instrument implementation for callable bonds
- :ref:`Callable.Factory <module-daml-finance-instrument-bond-v3-callable-factory-64026>`:
  Factory implementation to instantiate callable bonds
- :ref:`FixedRate.Instrument <module-daml-finance-instrument-bond-v3-fixedrate-instrument-89221>`:
  Instrument implementation for fixed-rate bonds
- :ref:`FixedRate.Factory <module-daml-finance-instrument-bond-v3-fixedrate-factory-55391>`:
  Factory implementation to instantiate fixed-rate bonds
- :ref:`FloatingRate.Instrument <module-daml-finance-instrument-bond-v3-floatingrate-instrument-82370>`:
  Instrument implementation for floating-rate bonds
- :ref:`FloatingRate.Factory <module-daml-finance-instrument-bond-v3-floatingrate-factory-96062>`:
  Factory implementation to instantiate floating-rate bonds
- :ref:`InflationLinked.Instrument <module-daml-finance-instrument-bond-v3-inflationlinked-instrument-99606>`:
  Instrument implementation for inflation-linked bonds
- :ref:`InflationLinked.Factory <module-daml-finance-instrument-bond-v3-inflationlinked-factory-84934>`:
  Factory implementation to instantiate inflation-linked bonds
- :ref:`ZeroCoupon.Instrument <module-daml-finance-instrument-bond-v3-zerocoupon-instrument-96672>`:
  Instrument implementation for zero-coupon bonds
- :ref:`ZeroCoupon.Factory <module-daml-finance-instrument-bond-v3-zerocoupon-factory-47672>`:
  Factory implementation to instantiate zero-coupon bonds
- :ref:`Util <module-daml-finance-instrument-bond-v3-util-25042>`:
  Bond-specific utility functions

Check out the page on
:doc:`How to use the Bond Instrument packages <../../instruments/bond>`
for a description of how to use these instruments in practice. There is also the tutorial
:doc:`How to implement a Contingent Claims-based instrument <../../tutorials/advanced-topics/instrument-modeling/contingent-claims-instrument>`,
which describes how the claims are defined and how the lifecycle interface is implemented for bonds.

Changelog
*********
