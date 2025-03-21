.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-22139:

Daml.Finance.Interface.Instrument.Bond.V3.ZeroCoupon.Factory
============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-factory-44152:

**interface** `Factory <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-factory-44152_>`_

  Factory interface to instantiate zero\-coupon bond instruments\.

  **viewtype** `V <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-v-51850_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-create-75151:

    **Choice** `Create <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-create-75151_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" zeroCoupon)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" zeroCoupon))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-i-7239>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - zeroCoupon
         - :ref:`ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978>`
         - Attributes to create a zero coupon bond\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-create-75151_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-i-7239>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-i-10477:

**type** `I <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-i-10477_>`_
  \= `Factory <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-factory-44152_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-v-51850:

**type** `V <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-v-51850_>`_
  \= `View <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-view-86374_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-factory-44152_>`_ `V <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-v-51850_>`_

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-view-86374:

**data** `View <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-view-86374_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-view-47045:

  `View <constr-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-view-47045_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-view-86374_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-view-86374_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-createtick-10941:

`create' <function-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-createtick-10941_>`_
  \: `Factory <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-factory-44152_>`_ \-\> `Create <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-create-75151_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-i-7239>`)
