.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-fixedrate-factory-45278:

Daml.Finance.Interface.Instrument.Bond.V3.FixedRate.Factory
===========================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-factory-74675:

**interface** `Factory <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-factory-74675_>`_

  Factory interface to instantiate fixed\-rate bond instruments\.

  **viewtype** `V <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-v-28577_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-create-25182:

    **Choice** `Create <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-create-25182_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" fixedRate)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" fixedRate))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-fixedrate-instrument-i-69642>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - fixedRate
         - :ref:`FixedRate <type-daml-finance-interface-instrument-bond-v3-fixedrate-types-fixedrate-8592>`
         - Attributes to create a fixed rate bond\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-create-25182_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-fixedrate-instrument-i-69642>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-i-96150:

**type** `I <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-i-96150_>`_
  \= `Factory <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-factory-74675_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-v-28577:

**type** `V <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-v-28577_>`_
  \= `View <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-view-22743_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-factory-74675_>`_ `V <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-v-28577_>`_

.. _type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-view-22743:

**data** `View <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-view-22743_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-bond-v3-fixedrate-factory-view-7714:

  `View <constr-daml-finance-interface-instrument-bond-v3-fixedrate-factory-view-7714_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-view-22743_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-view-22743_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-bond-v3-fixedrate-factory-createtick-38278:

`create' <function-daml-finance-interface-instrument-bond-v3-fixedrate-factory-createtick-38278_>`_
  \: `Factory <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-factory-74675_>`_ \-\> `Create <type-daml-finance-interface-instrument-bond-v3-fixedrate-factory-create-25182_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-fixedrate-instrument-i-69642>`)
