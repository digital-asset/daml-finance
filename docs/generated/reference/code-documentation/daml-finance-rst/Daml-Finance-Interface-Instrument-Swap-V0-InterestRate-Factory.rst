.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-interestrate-factory-59180:

Daml.Finance.Interface.Instrument.Swap.V0.InterestRate.Factory
==============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-factory-factory-42261:

**interface** `Factory <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-factory-42261_>`_

  Factory interface to instantiate interest rate swaps\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-v-87115_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-interestrate-factory-create-78116:

    **Choice** `Create <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-create-78116_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" interestRate)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" interestRate))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-i-87180>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - interestRate
         - :ref:`InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655>`
         - Attributes to create an interest rate swap\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-create-78116_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-i-87180>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-factory-i-6828:

**type** `I <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-i-6828_>`_
  \= `Factory <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-factory-42261_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-factory-v-87115:

**type** `V <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-v-87115_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-view-96657_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-factory-42261_>`_ `V <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-v-87115_>`_

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-factory-view-96657:

**data** `View <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-view-96657_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-interestrate-factory-view-77018:

  `View <constr-daml-finance-interface-instrument-swap-v0-interestrate-factory-view-77018_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-view-96657_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-view-96657_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-swap-v0-interestrate-factory-createtick-52356:

`create' <function-daml-finance-interface-instrument-swap-v0-interestrate-factory-createtick-52356_>`_
  \: `Factory <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-factory-42261_>`_ \-\> `Create <type-daml-finance-interface-instrument-swap-v0-interestrate-factory-create-78116_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-interestrate-instrument-i-87180>`)
