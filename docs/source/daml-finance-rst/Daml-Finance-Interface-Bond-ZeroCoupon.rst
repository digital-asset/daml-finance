.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-bond-zerocoupon-57257:

Module Daml.Finance.Interface.Bond.ZeroCoupon
=============================================

Interfaces
----------

.. _type-daml-finance-interface-bond-zerocoupon-factory-77382:

**interface** `Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382_>`_

  Interface that allows implementing templates to create instruments\.

  + **Choice Create**

    Create a new account\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
         - The instrument's key\.
       * - issueDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The date when the bond was issued\.
       * - maturityDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The last coupon date (and the redemption date) of the bond\.
       * - currency
         - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
         - The currency of the bond\. For example, if the bond pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - observers
         - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
         - The instrument's observers\.

  + **Choice Remove**

    Archive an account\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
         - The account's key\.

  + **Method asDisclosure \:**\ :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

    Conversion to ``Disclosure`` interface\.

  + **Method create' \:**\ Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`)

    Implementation of ``Create`` choice\.

  + **Method remove \:**\ Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

    Implementation of ``Remove`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-bond-zerocoupon-hasimplementation-17175:

**class** `Implementation <type-daml-finance-interface-bond-zerocoupon-implementation-72979_>`_ t \=\> `HasImplementation <class-daml-finance-interface-bond-zerocoupon-hasimplementation-17175_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-bond-zerocoupon-hasimplementation-17175_>`_ :ref:`Factory <type-daml-finance-bond-zerocoupon-factory-71815>`

  **instance** `HasImplementation <class-daml-finance-interface-bond-zerocoupon-hasimplementation-17175_>`_ `Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382_>`_

Data Types
----------

.. _type-daml-finance-interface-bond-zerocoupon-f-69872:

**type** `F <type-daml-finance-interface-bond-zerocoupon-f-69872_>`_
  \= `Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-bond-zerocoupon-implementation-72979:

**type** `Implementation <type-daml-finance-interface-bond-zerocoupon-implementation-72979_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)

  Type constraint used to require templates implementing ``Factory`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-bond-zerocoupon-view-57884:

**data** `View <type-daml-finance-interface-bond-zerocoupon-view-57884_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-bond-zerocoupon-view-23899:

  `View <constr-daml-finance-interface-bond-zerocoupon-view-23899_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-bond-zerocoupon-view-57884_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-bond-zerocoupon-view-57884_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-bond-zerocoupon-view-57884_>`_

  **instance** HasInterfaceView `Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382_>`_ `View <type-daml-finance-interface-bond-zerocoupon-view-57884_>`_

Functions
---------

.. _function-daml-finance-interface-bond-zerocoupon-asdisclosure-48392:

`asDisclosure <function-daml-finance-interface-bond-zerocoupon-asdisclosure-48392_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-bond-zerocoupon-createtick-14895:

`create' <function-daml-finance-interface-bond-zerocoupon-createtick-14895_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`)

.. _function-daml-finance-interface-bond-zerocoupon-remove-63077:

`remove <function-daml-finance-interface-bond-zerocoupon-remove-63077_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
