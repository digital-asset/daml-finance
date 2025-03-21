.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-holding-v4-factory-49942:

Daml.Finance.Interface.Holding.V4.Factory
=========================================

Interfaces
----------

.. _type-daml-finance-interface-holding-v4-factory-factory-22859:

**interface** `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_

  Holding factory contract used to create (credit) holdings\.

  **viewtype** `V <type-daml-finance-interface-holding-v4-factory-v-3225_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-holding-v4-factory-create-84550:

    **Choice** `Create <type-daml-finance-interface-holding-v4-factory-create-84550_>`_

    Create a holding on the instrument in the corresponding account\.

    Controller\: (DA\.Internal\.Record\.getField @\"custodian\" account), (DA\.Internal\.Record\.getField @\"owner\" account)

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument of which units are held\.
       * - account
         - :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`
         - The account at which the holding is held\. Defines the holding's owner and custodian\.
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Number of units\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - Observers of the holding to be credited\.

  + .. _type-daml-finance-interface-holding-v4-factory-getview-97414:

    **Choice** `GetView <type-daml-finance-interface-holding-v4-factory-getview-97414_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-holding-v4-factory-view-66511_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party fetching the view\.

  + .. _type-daml-finance-interface-holding-v4-factory-remove-83370:

    **Choice** `Remove <type-daml-finance-interface-holding-v4-factory-remove-83370_>`_

    Archive the factory\.

    Controller\: signatory this

    Returns\: ()

    (no fields)

  + **Method create' \:** `Create <type-daml-finance-interface-holding-v4-factory-create-84550_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`)

    Implementation of ``Create`` choice\.

  + **Method getKey \:** :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>`

    Get the unique key of the ``HoldingFactory``\.

Data Types
----------

.. _type-daml-finance-interface-holding-v4-factory-i-40318:

**type** `I <type-daml-finance-interface-holding-v4-factory-i-40318_>`_
  \= `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-holding-v4-factory-r-85845:

**type** `R <type-daml-finance-interface-holding-v4-factory-r-85845_>`_
  \= Reference

  Type synonym for ``Reference``\. This type is currently used as a work\-around given the lack of
  interface keys\.

.. _type-daml-finance-interface-holding-v4-factory-v-3225:

**type** `V <type-daml-finance-interface-holding-v4-factory-v-3225_>`_
  \= `View <type-daml-finance-interface-holding-v4-factory-view-66511_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_ `V <type-daml-finance-interface-holding-v4-factory-v-3225_>`_

.. _type-daml-finance-interface-holding-v4-factory-view-66511:

**data** `View <type-daml-finance-interface-holding-v4-factory-view-66511_>`_

  .. _constr-daml-finance-interface-holding-v4-factory-view-77806:

  `View <constr-daml-finance-interface-holding-v4-factory-view-77806_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Identifier for the holding factory\.

  **instance** HasInterfaceKey `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_ `View <type-daml-finance-interface-holding-v4-factory-view-66511_>`_ :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>` Reference GetCid SetCid SetObservers `GetView <type-daml-finance-interface-holding-v4-factory-getview-97414_>`_

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-holding-v4-factory-view-66511_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-holding-v4-factory-view-66511_>`_

Functions
---------

.. _function-daml-finance-interface-holding-v4-factory-tokey-36435:

`toKey <function-daml-finance-interface-holding-v4-factory-tokey-36435_>`_
  \: `View <type-daml-finance-interface-holding-v4-factory-view-66511_>`_ \-\> :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>`

  Convert the account's 'View' to its key\.

.. _function-daml-finance-interface-holding-v4-factory-holdingfactorykey-61521:

`holdingFactoryKey <function-daml-finance-interface-holding-v4-factory-holdingfactorykey-61521_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_ \=\> i \-\> :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>`

  Retrieves the key of a ``Factory``\.

.. _function-daml-finance-interface-holding-v4-factory-getkey-42011:

`getKey <function-daml-finance-interface-holding-v4-factory-getkey-42011_>`_
  \: `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_ \-\> :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>`

.. _function-daml-finance-interface-holding-v4-factory-createtick-50998:

`create' <function-daml-finance-interface-holding-v4-factory-createtick-50998_>`_
  \: `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_ \-\> `Create <type-daml-finance-interface-holding-v4-factory-create-84550_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`)

.. _function-daml-finance-interface-holding-v4-factory-createfactory-37513:

`createFactory <function-daml-finance-interface-holding-v4-factory-createfactory-37513_>`_
  \: (`HasCreate <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ f, `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ f `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_) \=\> f \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_)

  Create factory including reference\.

.. _function-daml-finance-interface-holding-v4-factory-disclose-27612:

`disclose <function-daml-finance-interface-holding-v4-factory-disclose-27612_>`_
  \: (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`) \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>` \-\> :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_)

  Disclose factory\.

.. _function-daml-finance-interface-holding-v4-factory-undisclose-93111:

`undisclose <function-daml-finance-interface-holding-v4-factory-undisclose-93111_>`_
  \: (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`) \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>` \-\> :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Factory <type-daml-finance-interface-holding-v4-factory-factory-22859_>`_))

  Undisclose factory\.

.. _function-daml-finance-interface-holding-v4-factory-exerciseinterfacebykey-335:

`exerciseInterfaceByKey <function-daml-finance-interface-holding-v4-factory-exerciseinterfacebykey-335_>`_
  \: (`HasInterfaceTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfacetyperep-84221>`_ i, `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ i c r) \=\> :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> c \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ r

  Exercise interface by key\.
  This method can be used to exercise a choice on a ``Factory`` given its ``HoldingFactoryKey``\.
  Requires as input the ``HoldingFactoryKey``, the actor exercising the choice, and the choice
  arguments\. For example\:

  .. code-block:: daml

    exerciseInterfaceByKey @HoldingFactory.I holdingFactoryKey actor
      HoldingFactory.Create with instrument; account; amount; observers
