.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-zerocoupon-types-44506:

Daml.Finance.Interface.Instrument.Bond.V3.ZeroCoupon.Types
==========================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978:

**data** `ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978_>`_

  Describes the attributes of a Zero Coupon bond\.

  .. _constr-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-50769:

  `ZeroCoupon <constr-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-50769_>`_

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
         - The description of the bond\.
       * - issueDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The date when the bond was issued\.
       * - maturityDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The redemption date of the bond\.
       * - currency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The currency of the bond\. For example, if the bond pays in USD this should be a USD cash instrument\.
       * - notional
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The notional of the bond\. This is the face value corresponding to one unit of the bond instrument\. For example, if one bond unit corresponds to 1000 USD, this should be 1000\.0\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978_>`_
