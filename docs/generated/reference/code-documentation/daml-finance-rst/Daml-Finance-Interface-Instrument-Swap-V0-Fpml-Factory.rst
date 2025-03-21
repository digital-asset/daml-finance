.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-fpml-factory-27095:

Daml.Finance.Interface.Instrument.Swap.V0.Fpml.Factory
======================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-fpml-factory-factory-93528:

**interface** `Factory <type-daml-finance-interface-instrument-swap-v0-fpml-factory-factory-93528_>`_

  Factory interface to instantiate FpML swaps\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-fpml-factory-v-14730_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327:

    **Choice** `Create <type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" fpml)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" fpml))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-i-31607>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - fpml
         - :ref:`Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949>`
         - Attributes to create a swap specified as FpML swapStreams\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-i-31607>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-fpml-factory-i-73357:

**type** `I <type-daml-finance-interface-instrument-swap-v0-fpml-factory-i-73357_>`_
  \= `Factory <type-daml-finance-interface-instrument-swap-v0-fpml-factory-factory-93528_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-swap-v0-fpml-factory-v-14730:

**type** `V <type-daml-finance-interface-instrument-swap-v0-fpml-factory-v-14730_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-fpml-factory-view-62118_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-swap-v0-fpml-factory-factory-93528_>`_ `V <type-daml-finance-interface-instrument-swap-v0-fpml-factory-v-14730_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-factory-view-62118:

**data** `View <type-daml-finance-interface-instrument-swap-v0-fpml-factory-view-62118_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-factory-view-48881:

  `View <constr-daml-finance-interface-instrument-swap-v0-fpml-factory-view-48881_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-fpml-factory-view-62118_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-fpml-factory-view-62118_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-swap-v0-fpml-factory-createtick-69493:

`create' <function-daml-finance-interface-instrument-swap-v0-fpml-factory-createtick-69493_>`_
  \: `Factory <type-daml-finance-interface-instrument-swap-v0-fpml-factory-factory-93528_>`_ \-\> `Create <type-daml-finance-interface-instrument-swap-v0-fpml-factory-create-21327_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-fpml-instrument-i-31607>`)
