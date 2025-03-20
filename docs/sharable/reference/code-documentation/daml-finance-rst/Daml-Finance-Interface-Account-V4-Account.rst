.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-account-v4-account-30007:

Daml.Finance.Interface.Account.V4.Account
=========================================

We recommend to import this module qualified\.

Interfaces
----------

.. _type-daml-finance-interface-account-v4-account-account-93407:

**interface** `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_

  An interface which represents an established relationship between a provider and an owner\.

  **viewtype** `V <type-daml-finance-interface-account-v4-account-v-29510_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-account-v4-account-credit-92816:

    **Choice** `Credit <type-daml-finance-interface-account-v4-account-credit-92816_>`_

    Creates a new ``Holding`` in the corresponding ``Account``\.

    Controller\: (DA\.Internal\.Record\.getField @\"custodian\" (view this)), (DA\.Internal\.Record\.getField @\"incoming\" (DA\.Internal\.Record\.getField @\"controllers\" (view this)))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - quantity
         - :ref:`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585>` :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The target ``Instrument`` and corresponding amount\.

  + .. _type-daml-finance-interface-account-v4-account-debit-98062:

    **Choice** `Debit <type-daml-finance-interface-account-v4-account-debit-98062_>`_

    Removes an existing ``Holding``\.

    Controller\: (DA\.Internal\.Record\.getField @\"custodian\" (view this)), (DA\.Internal\.Record\.getField @\"outgoing\" (DA\.Internal\.Record\.getField @\"controllers\" (view this)))

    Returns\: ()

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - holdingCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`
         - The ``Holding``'s contract id\.

  + .. _type-daml-finance-interface-account-v4-account-getview-21073:

    **Choice** `GetView <type-daml-finance-interface-account-v4-account-getview-21073_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-account-v4-account-view-18066_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party fetching the view\.

  + .. _type-daml-finance-interface-account-v4-account-remove-91479:

    **Choice** `Remove <type-daml-finance-interface-account-v4-account-remove-91479_>`_

    Archive the account\.

    Controller\: signatory this

    Returns\: ()

    (no fields)

  + **Method credit \:** `Credit <type-daml-finance-interface-account-v4-account-credit-92816_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`)

    Implementation of the ``Credit`` choice\.

  + **Method debit \:** `Debit <type-daml-finance-interface-account-v4-account-debit-98062_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

    Implementation of the ``Debit`` choice\.

  + **Method getKey \:** :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`

    Get the unique key of the ``Account``\.

Data Types
----------

.. _type-daml-finance-interface-account-v4-account-controllers-59817:

**data** `Controllers <type-daml-finance-interface-account-v4-account-controllers-59817_>`_

  Controllers of the account (related to transfers)\.

  .. _constr-daml-finance-interface-account-v4-account-controllers-11570:

  `Controllers <constr-daml-finance-interface-account-v4-account-controllers-11570_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - outgoing
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties instructing a transfer (outgoing)\.
       * - incoming
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties approving a transfer (incoming)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Controllers <type-daml-finance-interface-account-v4-account-controllers-59817_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Controllers <type-daml-finance-interface-account-v4-account-controllers-59817_>`_

.. _type-daml-finance-interface-account-v4-account-i-22897:

**type** `I <type-daml-finance-interface-account-v4-account-i-22897_>`_
  \= `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_

  Type synonym for ``Account``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-account-v4-factory-factory-48831>` \"create'\" (:ref:`Create <type-daml-finance-interface-account-v4-factory-create-72130>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-account-v4-account-i-22897_>`_))

.. _type-daml-finance-interface-account-v4-account-r-46890:

**type** `R <type-daml-finance-interface-account-v4-account-r-46890_>`_
  \= Reference

  Type synonym for ``Reference``\. This type is currently used as a work\-around given the lack of
  interface keys\.

.. _type-daml-finance-interface-account-v4-account-v-29510:

**type** `V <type-daml-finance-interface-account-v4-account-v-29510_>`_
  \= `View <type-daml-finance-interface-account-v4-account-view-18066_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_ `V <type-daml-finance-interface-account-v4-account-v-29510_>`_

.. _type-daml-finance-interface-account-v4-account-view-18066:

**data** `View <type-daml-finance-interface-account-v4-account-view-18066_>`_

  View for ``Account``\.

  .. _constr-daml-finance-interface-account-v4-account-view-46419:

  `View <constr-daml-finance-interface-account-v4-account-view-46419_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - custodian
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing accounting services\.
       * - owner
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party owning this account\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Identifier for the account\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Human readable description of the account\.
       * - controllers
         - `Controllers <type-daml-finance-interface-account-v4-account-controllers-59817_>`_
         - Parties controlling transfers\.

  **instance** HasInterfaceKey `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_ `View <type-daml-finance-interface-account-v4-account-view-18066_>`_ :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>` Reference GetCid SetCid SetObservers `GetView <type-daml-finance-interface-account-v4-account-getview-21073_>`_

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-account-v4-account-view-18066_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-account-v4-account-view-18066_>`_

Functions
---------

.. _function-daml-finance-interface-account-v4-account-tokey-30232:

`toKey <function-daml-finance-interface-account-v4-account-tokey-30232_>`_
  \: `View <type-daml-finance-interface-account-v4-account-view-18066_>`_ \-\> :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`

  Convert the account's 'View' to its key\.

.. _function-daml-finance-interface-account-v4-account-accountkey-74157:

`accountKey <function-daml-finance-interface-account-v4-account-accountkey-74157_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_ \=\> i \-\> :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`

  Retrieves the key of an ``Account``\.

.. _function-daml-finance-interface-account-v4-account-getkey-54894:

`getKey <function-daml-finance-interface-account-v4-account-getkey-54894_>`_
  \: `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_ \-\> :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`

.. _function-daml-finance-interface-account-v4-account-credit-27484:

`credit <function-daml-finance-interface-account-v4-account-credit-27484_>`_
  \: `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_ \-\> `Credit <type-daml-finance-interface-account-v4-account-credit-92816_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`)

.. _function-daml-finance-interface-account-v4-account-debit-31346:

`debit <function-daml-finance-interface-account-v4-account-debit-31346_>`_
  \: `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_ \-\> `Debit <type-daml-finance-interface-account-v4-account-debit-98062_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

.. _function-daml-finance-interface-account-v4-account-disclose-16489:

`disclose <function-daml-finance-interface-account-v4-account-disclose-16489_>`_
  \: (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`) \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>` \-\> :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_)

  Disclose account\.

.. _function-daml-finance-interface-account-v4-account-undisclose-26330:

`undisclose <function-daml-finance-interface-account-v4-account-undisclose-26330_>`_
  \: (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`) \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>` \-\> :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Account <type-daml-finance-interface-account-v4-account-account-93407_>`_))

  Undisclose account\.

.. _function-daml-finance-interface-account-v4-account-exerciseinterfacebykey-87310:

`exerciseInterfaceByKey <function-daml-finance-interface-account-v4-account-exerciseinterfacebykey-87310_>`_
  \: (`HasInterfaceTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfacetyperep-84221>`_ i, `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ i c r) \=\> :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> c \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ r

  Exercise interface by key\.
  This method can be used to exercise a choice on an ``Account`` given its ``AccountKey``\.
  Requires as input the ``AccountKey``,
  For example\:

  .. code-block:: daml

    exerciseInterfaceByKey @Account.I accountKey actor Account.Debit with holdingCid
