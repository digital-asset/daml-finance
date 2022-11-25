.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Instrument.Bond
############################

This package contains the *implementation* of different bond types, defined in the following
modules:

- :ref:`FixedRate.Instrument <module-daml-finance-instrument-bond-fixedrate-instrument-67993>`:
  Instrument implementation for fixed-rate bonds
- :ref:`FixedRate.Factory <module-daml-finance-instrument-bond-fixedrate-factory-46203>`:
  Factory implementation to instantiate fixed-rate bonds
- :ref:`FloatingRate.Instrument <module-daml-finance-instrument-bond-floatingrate-instrument-98586>`:
  Instrument implementation for floating-rate bonds
- :ref:`FloatingRate.Factory <module-daml-finance-instrument-bond-floatingrate-factory-64782>`:
  Factory implementation to instantiate floating-rate bonds
- :ref:`InflationLinked.Instrument <module-daml-finance-instrument-bond-inflationlinked-instrument-30250>`:
  Instrument implementation for inflation-linked bonds
- :ref:`InflationLinked.Factory <module-daml-finance-instrument-bond-inflationlinked-factory-70614>`:
  Factory implementation to instantiate inflation-linked bonds
- :ref:`ZeroCoupon.Instrument <module-daml-finance-instrument-bond-zerocoupon-instrument-52804>`:
  Instrument implementation for zero-coupon bonds
- :ref:`ZeroCoupon.Factory <module-daml-finance-instrument-bond-zerocoupon-factory-51640>`:
  Factory implementation to instantiate zero-coupon bonds

Check out the tutorial on
:doc:`How to use the Bond extension package <../../tutorials/instrument-modeling/bond-extension>`
for a description of how to use the bond extension in practice. There is also the tutorial
:doc:`How to implement a Contingent Claims-based instrument <../../tutorials/instrument-modeling/contingent-claims-instrument>`,
which describes how the claims are defined and how the lifecycle interface is implemented for
bonds.
