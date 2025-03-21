.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-core-v3-builders-35188:

ContingentClaims.Core.V3.Builders
=================================

Functions
---------

.. _function-contingentclaims-core-v3-builders-unrolldates-49024:

`unrollDates <function-contingentclaims-core-v3-builders-unrolldates-49024_>`_
  \: `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_ \-\> `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_ \-\> \[`Month <https://docs.daml.com/daml/stdlib/DA-Date.html#type-da-date-types-month-22803>`_\] \-\> `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_ \-\> \[`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_\]

  Helper function to generate a series of fixing dates, e\.g\. for coupon payments in ``fixed``\.
  This assumes ``fixingMonths`` and ``fixingDates`` are ordered\.
  The Daml Finance library(https://github.com/digital-asset/daml-finance) has more
  feature\-complete date handling functions\.

.. _function-contingentclaims-core-v3-builders-forward-17460:

`forward <function-contingentclaims-core-v3-builders-forward-17460_>`_
  \: t \-\> :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Forward agreement\. Discounted by (potentially stochastic) interest rate ``r``\.

.. _function-contingentclaims-core-v3-builders-fra-47282:

`fra <function-contingentclaims-core-v3-builders-fra-47282_>`_
  \: t \-\> t \-\> :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o \-\> :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Forward rate agreement\.

.. _function-contingentclaims-core-v3-builders-zcb-50130:

`zcb <function-contingentclaims-core-v3-builders-zcb-50130_>`_
  \: t \-\> x \-\> ccy \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x ccy o

  Zero Coupon Bond\.

.. _function-contingentclaims-core-v3-builders-floating-48956:

`floating <function-contingentclaims-core-v3-builders-floating-48956_>`_
  \: :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o \-\> :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o \-\> ccy \-\> \[t\] \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x ccy o

  A floating rate bond\.

.. _function-contingentclaims-core-v3-builders-fixed-40239:

`fixed <function-contingentclaims-core-v3-builders-fixed-40239_>`_
  \: x \-\> x \-\> ccy \-\> \[t\] \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x ccy o

  A (fixed rate) coupon paying bond\.

.. _function-contingentclaims-core-v3-builders-european-38509:

`european <function-contingentclaims-core-v3-builders-european-38509_>`_
  \: t \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  European option on the input claim\. At maturity, the holder must ``EXERCISE`` or ``EXPIRE``
  the claim\. e\.g\. call option on S&P 500\:

  .. code-block:: daml

    european (date 2021 05 14) (observe "SPX" - pure 4200)

.. _function-contingentclaims-core-v3-builders-bermudan-53632:

`bermudan <function-contingentclaims-core-v3-builders-bermudan-53632_>`_
  \: \[t\] \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Bermudan option on the input claim\. Given a pre\-defined set of times
  {t\_1, t\_2, \.\., t\_N}, it allows the holder to acquire the underlying claim on at
  most one of these times\.
  At each election time before maturity, the holder must ``EXERCISE`` the option or ``POSTPONE``\.
  At maturity, the holder must ``EXERCISE`` or ``EXPIRE``\.

.. _function-contingentclaims-core-v3-builders-american-12480:

`american <function-contingentclaims-core-v3-builders-american-12480_>`_
  \: t \-\> t \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  American option (knock\-in)\. The lead parameter is the first possible acquisition date\.

.. _function-contingentclaims-core-v3-builders-swap-11951:

`swap <function-contingentclaims-core-v3-builders-swap-11951_>`_
  \: (\[t\] \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o) \-\> (\[t\] \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o) \-\> \[t\] \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Asset swap on specific fixing dates ``[t]``\. For example\:

  .. code-block:: daml

    fixedUsdVsFloatingEur : [t] -> Serializable.Claim Text
    fixedUsdVsFloatingEur =
      fixed 100.0 0.02 "USD" `swap` floating (observe "USDEUR" * pure 100.0) (observe "EUR1M") "EUR"
