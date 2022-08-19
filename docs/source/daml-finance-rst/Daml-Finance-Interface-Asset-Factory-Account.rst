.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-factory-account-25735:

Module Daml.Finance.Interface.Asset.Factory.Account
===================================================

Interfaces
----------

.. _type-daml-finance-interface-asset-factory-account-factory-23412:

**interface** `Factory <type-daml-finance-interface-asset-factory-account-factory-23412_>`_

  Interface that allows implementing templates to create accounts\.
  
  + **Choice Create**
    
    Create a new account\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - account
         - :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
         - The account's key\.
       * - holdingFactoryCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`F <type-daml-finance-interface-asset-factory-holding-f-78374>`
         - Associated holding factory for the account\.
       * - observers
         - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
         - The account's observers\.
  
  + **Choice Remove**
    
    Archive an account\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - account
         - :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
         - The account's key\.
  
  + **Method asDisclosure \:** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
    
    Conversion to ``Disclosure`` interface\.
  
  + **Method create' \:** Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-account-i-38237>`)
    
    Implementation of ``Create`` choice\.
  
  + **Method remove \:** Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
    
    Implementation of ``Remove`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-factory-account-hasimplementation-22577:

**class** `Implementation <type-daml-finance-interface-asset-factory-account-implementation-47641_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-factory-account-hasimplementation-22577_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-asset-factory-account-hasimplementation-22577_>`_ :ref:`Factory <type-daml-finance-asset-account-factory-10857>`
  
  **instance** `HasImplementation <class-daml-finance-interface-asset-factory-account-hasimplementation-22577_>`_ `F <type-daml-finance-interface-asset-factory-account-f-54942_>`_

Data Types
----------

.. _type-daml-finance-interface-asset-factory-account-f-54942:

**type** `F <type-daml-finance-interface-asset-factory-account-f-54942_>`_
  \= `Factory <type-daml-finance-interface-asset-factory-account-factory-23412_>`_
  
  Type synonym for ``Factory``\.
  
  **instance** `HasImplementation <class-daml-finance-interface-asset-factory-account-hasimplementation-22577_>`_ `F <type-daml-finance-interface-asset-factory-account-f-54942_>`_
  
  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-asset-account-factory-10857>` `F <type-daml-finance-interface-asset-factory-account-f-54942_>`_
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-asset-account-factory-10857>` `F <type-daml-finance-interface-asset-factory-account-f-54942_>`_
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `F <type-daml-finance-interface-asset-factory-account-f-54942_>`_ :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _type-daml-finance-interface-asset-factory-account-implementation-47641:

**type** `Implementation <type-daml-finance-interface-asset-factory-account-implementation-47641_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `F <type-daml-finance-interface-asset-factory-account-f-54942_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Factory`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-asset-factory-account-view-96890:

**data** `View <type-daml-finance-interface-asset-factory-account-view-96890_>`_

  .. _constr-daml-finance-interface-asset-factory-account-view-20025:
  
  `View <constr-daml-finance-interface-asset-factory-account-view-20025_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-factory-account-view-96890_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-asset-factory-account-view-96890_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-factory-account-view-96890_>`_
  
  **instance** `HasInterfaceView <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfaceview-4492>`_ `Factory <type-daml-finance-interface-asset-factory-account-factory-23412_>`_ `View <type-daml-finance-interface-asset-factory-account-view-96890_>`_

Functions
---------

.. _function-daml-finance-interface-asset-factory-account-asdisclosure-24838:

`asDisclosure <function-daml-finance-interface-asset-factory-account-asdisclosure-24838_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-account-factory-23412_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-asset-factory-account-createtick-14857:

`create' <function-daml-finance-interface-asset-factory-account-createtick-14857_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-account-factory-23412_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-account-i-38237>`)

.. _function-daml-finance-interface-asset-factory-account-remove-40611:

`remove <function-daml-finance-interface-asset-factory-account-remove-40611_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-account-factory-23412_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
