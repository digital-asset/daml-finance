.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-asset-factory-15289:

Daml.Finance.Interface.Instrument.Swap.V0.Asset.Factory
=======================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-asset-factory-factory-42186:

**interface** `Factory <type-daml-finance-interface-instrument-swap-v0-asset-factory-factory-42186_>`_

  Factory interface to instantiate asset swaps\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-asset-factory-v-25068_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-asset-factory-create-72901:

    **Choice** `Create <type-daml-finance-interface-instrument-swap-v0-asset-factory-create-72901_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" asset)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" asset))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-asset-instrument-i-95573>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - asset
         - :ref:`Asset <type-daml-finance-interface-instrument-swap-v0-asset-types-asset-43409>`
         - Attributes to create an asset swap\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-swap-v0-asset-factory-create-72901_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-asset-instrument-i-95573>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-asset-factory-i-5355:

**type** `I <type-daml-finance-interface-instrument-swap-v0-asset-factory-i-5355_>`_
  \= `Factory <type-daml-finance-interface-instrument-swap-v0-asset-factory-factory-42186_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-swap-v0-asset-factory-v-25068:

**type** `V <type-daml-finance-interface-instrument-swap-v0-asset-factory-v-25068_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-asset-factory-view-92696_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-swap-v0-asset-factory-factory-42186_>`_ `V <type-daml-finance-interface-instrument-swap-v0-asset-factory-v-25068_>`_

.. _type-daml-finance-interface-instrument-swap-v0-asset-factory-view-92696:

**data** `View <type-daml-finance-interface-instrument-swap-v0-asset-factory-view-92696_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-asset-factory-view-94505:

  `View <constr-daml-finance-interface-instrument-swap-v0-asset-factory-view-94505_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-asset-factory-view-92696_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-asset-factory-view-92696_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-swap-v0-asset-factory-createtick-39315:

`create' <function-daml-finance-interface-instrument-swap-v0-asset-factory-createtick-39315_>`_
  \: `Factory <type-daml-finance-interface-instrument-swap-v0-asset-factory-factory-42186_>`_ \-\> `Create <type-daml-finance-interface-instrument-swap-v0-asset-factory-create-72901_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-asset-instrument-i-95573>`)
