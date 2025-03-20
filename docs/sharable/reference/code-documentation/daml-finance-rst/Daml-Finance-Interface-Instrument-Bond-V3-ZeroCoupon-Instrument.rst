.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-34777:

Daml.Finance.Interface.Instrument.Bond.V3.ZeroCoupon.Instrument
===============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-instrument-79974:

**interface** `Instrument <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-instrument-79974_>`_

  Instrument interface representing a zero coupon bond\.

  **viewtype** `V <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-v-61712_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-getview-89719:

    **Choice** `GetView <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-getview-89719_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-v-61712_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.


Data Types
----------

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-i-7239:

**type** `I <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-i-7239_>`_
  \= `Instrument <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-instrument-79974_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-factory-44152>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-bond-v3-zerocoupon-factory-create-75151>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-i-7239_>`_))

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-v-61712:

**type** `V <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-v-61712_>`_
  \= `View <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-view-64748_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-instrument-79974_>`_ `V <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-v-61712_>`_

.. _type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-view-64748:

**data** `View <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-view-64748_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-view-5977:

  `View <constr-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-view-5977_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - zeroCoupon
         - :ref:`ZeroCoupon <type-daml-finance-interface-instrument-bond-v3-zerocoupon-types-zerocoupon-3978>`
         - Attributes of a zero coupon bond\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-view-64748_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-v3-zerocoupon-instrument-view-64748_>`_
