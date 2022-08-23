.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-bond-zerocoupon-72656:

Module Daml.Finance.Instrument.Bond.ZeroCoupon
==============================================

Templates
---------

.. _type-daml-finance-instrument-bond-zerocoupon-factory-42045:

**template** `Factory <type-daml-finance-instrument-bond-zerocoupon-factory-42045_>`_

  Factory template for instrument creation\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The factory's provider\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - The factory's observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  + **implements** :ref:`Factory <type-daml-finance-interface-instrument-bond-zerocoupon-factory-76014>`

.. _type-daml-finance-instrument-bond-zerocoupon-instrument-46935:

**template** `Instrument <type-daml-finance-instrument-bond-zerocoupon-instrument-46935_>`_

  This template models a zero coupon bond\.
  It does not pay any coupons, only the redemption amount at maturity\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The depository of the instrument\.
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The issuer of the instrument\.
     * - id
       - :ref:`Id <type-daml-finance-interface-common-types-id-88316>`
       - An identifier of the instrument\.
     * - issueDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The date when the bond was issued\.
     * - maturityDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The last coupon date (and the redemption date) of the bond\.
     * - currency
       - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-58546>`
       - The currency of the bond\. For example, if the bond pays in USD this should be a USD cash instrument\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - The observers of the instrument\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  + **implements** :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-67236>`
  
  + **implements** :ref:`I <type-daml-finance-interface-instrument-generic-hasclaims-i-36868>`
  
  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-instrument-bond-zerocoupon-t-14629:

**type** `T <type-daml-finance-instrument-bond-zerocoupon-t-14629_>`_
  \= `Instrument <type-daml-finance-instrument-bond-zerocoupon-instrument-46935_>`_
