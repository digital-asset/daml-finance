.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-bond-zerocoupon-55118:

Module Daml.Finance.Bond.ZeroCoupon
===================================

Templates
---------

.. _type-daml-finance-bond-zerocoupon-factory-71815:

**template** `Factory <type-daml-finance-bond-zerocoupon-factory-71815_>`_

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
    

  + **implements** :ref:`Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382>`
  
  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _type-daml-finance-bond-zerocoupon-instrument-49917:

**template** `Instrument <type-daml-finance-bond-zerocoupon-instrument-49917_>`_

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
       - :ref:`Id <type-daml-finance-interface-asset-types-id-89116>`
       - An identifier of the instrument\.
     * - issueDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The date when the bond was issued\.
     * - maturityDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The last coupon date (and the redemption date) of the bond\.
     * - currency
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - The currency of the bond\. For example, if the bond pays in USD this should be a USD cash instrument\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - The observers of the instrument\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`
  
  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  + **implements** :ref:`I <type-daml-finance-interface-derivative-hasclaims-i-90893>`
  
  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-bond-zerocoupon-t-92363:

**type** `T <type-daml-finance-bond-zerocoupon-t-92363_>`_
  \= `Instrument <type-daml-finance-bond-zerocoupon-instrument-49917_>`_
