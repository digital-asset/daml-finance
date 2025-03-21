.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-callable-factory-33809:

Daml.Finance.Interface.Instrument.Bond.V3.Callable.Factory
==========================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-v3-callable-factory-factory-95382:

**interface** `Factory <type-daml-finance-interface-instrument-bond-v3-callable-factory-factory-95382_>`_

  Factory interface to instantiate callable bond instruments\.

  **viewtype** `V <type-daml-finance-interface-instrument-bond-v3-callable-factory-v-96912_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-bond-v3-callable-factory-create-44265:

    **Choice** `Create <type-daml-finance-interface-instrument-bond-v3-callable-factory-create-44265_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" callable)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" callable))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-callable-instrument-i-23721>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - callable
         - :ref:`Callable <type-daml-finance-interface-instrument-bond-v3-callable-types-callable-12794>`
         - Attributes to create a callable rate bond\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-bond-v3-callable-factory-create-44265_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-callable-instrument-i-23721>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-v3-callable-factory-i-42439:

**type** `I <type-daml-finance-interface-instrument-bond-v3-callable-factory-i-42439_>`_
  \= `Factory <type-daml-finance-interface-instrument-bond-v3-callable-factory-factory-95382_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-bond-v3-callable-factory-v-96912:

**type** `V <type-daml-finance-interface-instrument-bond-v3-callable-factory-v-96912_>`_
  \= `View <type-daml-finance-interface-instrument-bond-v3-callable-factory-view-21804_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-bond-v3-callable-factory-factory-95382_>`_ `V <type-daml-finance-interface-instrument-bond-v3-callable-factory-v-96912_>`_

.. _type-daml-finance-interface-instrument-bond-v3-callable-factory-view-21804:

**data** `View <type-daml-finance-interface-instrument-bond-v3-callable-factory-view-21804_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-bond-v3-callable-factory-view-4775:

  `View <constr-daml-finance-interface-instrument-bond-v3-callable-factory-view-4775_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-v3-callable-factory-view-21804_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-v3-callable-factory-view-21804_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-bond-v3-callable-factory-createtick-18487:

`create' <function-daml-finance-interface-instrument-bond-v3-callable-factory-createtick-18487_>`_
  \: `Factory <type-daml-finance-interface-instrument-bond-v3-callable-factory-factory-95382_>`_ \-\> `Create <type-daml-finance-interface-instrument-bond-v3-callable-factory-create-44265_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-callable-instrument-i-23721>`)
