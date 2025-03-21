.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-account-v4-factory-69358:

Daml.Finance.Interface.Account.V4.Factory
=========================================

Interfaces
----------

.. _type-daml-finance-interface-account-v4-factory-factory-48831:

**interface** `Factory <type-daml-finance-interface-account-v4-factory-factory-48831_>`_

  Interface that allows implementing templates to create accounts\.

  **viewtype** `V <type-daml-finance-interface-account-v4-factory-v-97013_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-account-v4-factory-create-72130:

    **Choice** `Create <type-daml-finance-interface-account-v4-factory-create-72130_>`_

    Create a new account\.

    Controller\: (DA\.Internal\.Record\.getField @\"custodian\" account), (DA\.Internal\.Record\.getField @\"owner\" account)

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-account-v4-account-i-22897>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - account
         - :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`
         - The account's key\.
       * - holdingFactory
         - :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>`
         - Associated holding factory for the account\.
       * - controllers
         - :ref:`Controllers <type-daml-finance-interface-account-v4-account-controllers-59817>`
         - Controllers of the account\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Human readable description of the account\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The account's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-account-v4-factory-create-72130_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-account-v4-account-i-22897>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-account-v4-factory-i-68866:

**type** `I <type-daml-finance-interface-account-v4-factory-i-68866_>`_
  \= `Factory <type-daml-finance-interface-account-v4-factory-factory-48831_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-account-v4-factory-v-97013:

**type** `V <type-daml-finance-interface-account-v4-factory-v-97013_>`_
  \= `View <type-daml-finance-interface-account-v4-factory-view-19547_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-account-v4-factory-factory-48831_>`_ `V <type-daml-finance-interface-account-v4-factory-v-97013_>`_

.. _type-daml-finance-interface-account-v4-factory-view-19547:

**data** `View <type-daml-finance-interface-account-v4-factory-view-19547_>`_

  .. _constr-daml-finance-interface-account-v4-factory-view-94498:

  `View <constr-daml-finance-interface-account-v4-factory-view-94498_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-account-v4-factory-view-19547_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-account-v4-factory-view-19547_>`_

Functions
---------

.. _function-daml-finance-interface-account-v4-factory-createtick-33466:

`create' <function-daml-finance-interface-account-v4-factory-createtick-33466_>`_
  \: `Factory <type-daml-finance-interface-account-v4-factory-factory-48831_>`_ \-\> `Create <type-daml-finance-interface-account-v4-factory-create-72130_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-account-v4-account-i-22897>`)
