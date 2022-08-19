.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-account-35247:

Module Daml.Finance.Interface.Asset.Account
===========================================

We recommend to import this module qualified\.

Interfaces
----------

.. _type-daml-finance-interface-asset-account-account-19971:

**interface** `Account <type-daml-finance-interface-asset-account-account-19971_>`_

  An interface which represents an established relationship between a provider and an owner\.
  
  + **Choice Credit**
    
    Creates a new ``Holding`` in the corresponding ``Account``\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - quantity
         - :ref:`Q <type-daml-finance-interface-asset-instrument-q-31714>`
         - The target ``Instrument`` and corresponding amount\.
  
  + **Choice Debit**
    
    Destroys an existing ``Holding``\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - holdingCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`
         - The ``Holding``'s contract id\.
  
  + **Choice GetView**
    
    Returns the account's view\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party fetching the view\.
  
  + **Method asDisclosure \:** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
    
    Conversion to ``Disclosure`` interface\.
  
  + **Method credit \:** Credit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`)
    
    Implementation of the ``Credit`` choice\.
  
  + **Method debit \:** Debit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
    
    Implementation of the ``Debit`` choice\.
  
  + **Method getKey \:** :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
    
    Get the unique key of the ``Account``\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-account-hasimplementation-58285:

**class** `Implementation <type-daml-finance-interface-asset-account-implementation-37277_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-account-hasimplementation-58285_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-asset-account-hasimplementation-58285_>`_ :ref:`T <type-daml-finance-asset-account-t-52313>`
  
  **instance** `HasImplementation <class-daml-finance-interface-asset-account-hasimplementation-58285_>`_ `I <type-daml-finance-interface-asset-account-i-38237_>`_

Data Types
----------

.. _type-daml-finance-interface-asset-account-i-38237:

**type** `I <type-daml-finance-interface-asset-account-i-38237_>`_
  \= `Account <type-daml-finance-interface-asset-account-account-19971_>`_
  
  Type synonym for ``Account``\.
  
  **instance** `HasImplementation <class-daml-finance-interface-asset-account-hasimplementation-58285_>`_ `I <type-daml-finance-interface-asset-account-i-38237_>`_
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-asset-factory-account-factory-23412>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-account-i-38237_>`_))
  
  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Account <type-daml-finance-asset-account-account-64286>` `I <type-daml-finance-interface-asset-account-i-38237_>`_
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Account <type-daml-finance-asset-account-account-64286>` `I <type-daml-finance-interface-asset-account-i-38237_>`_
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-asset-account-i-38237_>`_ :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Factory <type-daml-finance-interface-asset-factory-account-factory-23412>`) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t Create (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-account-i-38237_>`_)
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Factory <type-daml-finance-interface-asset-factory-account-factory-23412>`) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t Create (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-account-i-38237_>`_)
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Factory <type-daml-finance-interface-asset-factory-account-factory-23412>` \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t Create (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-account-i-38237_>`_)
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Factory <type-daml-finance-interface-asset-factory-account-factory-23412>` \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t Create (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-account-i-38237_>`_)

.. _type-daml-finance-interface-asset-account-implementation-37277:

**type** `Implementation <type-daml-finance-interface-asset-account-implementation-37277_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-asset-account-i-38237_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Account`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-asset-account-k-29547:

**type** `K <type-daml-finance-interface-asset-account-k-29547_>`_
  \= :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
  
  Type synonym for ``AccountKey``\.

.. _type-daml-finance-interface-asset-account-r-23190:

**type** `R <type-daml-finance-interface-asset-account-r-23190_>`_
  \= Reference
  
  Type synonym for ``Reference``\. This type is currently used as a work\-around given the lack of interface keys\.

.. _type-daml-finance-interface-asset-account-v-40570:

**type** `V <type-daml-finance-interface-asset-account-v-40570_>`_
  \= `View <type-daml-finance-interface-asset-account-view-6934_>`_
  
  Type synonym for ``View``\.

.. _type-daml-finance-interface-asset-account-view-6934:

**data** `View <type-daml-finance-interface-asset-account-view-6934_>`_

  View for ``Account``\.
  
  .. _constr-daml-finance-interface-asset-account-view-21713:
  
  `View <constr-daml-finance-interface-asset-account-view-21713_>`_
  
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
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Textual description of the account\.
       * - holdingFactoryCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`F <type-daml-finance-interface-asset-factory-holding-f-78374>`
         - Associated holding factory\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-account-view-6934_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-asset-account-view-6934_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-account-view-6934_>`_
  
  **instance** `HasInterfaceView <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfaceview-4492>`_ `Account <type-daml-finance-interface-asset-account-account-19971_>`_ `View <type-daml-finance-interface-asset-account-view-6934_>`_
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Account <type-daml-finance-interface-asset-account-account-19971_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t GetView `View <type-daml-finance-interface-asset-account-view-6934_>`_
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Account <type-daml-finance-interface-asset-account-account-19971_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t GetView `View <type-daml-finance-interface-asset-account-view-6934_>`_
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Account <type-daml-finance-interface-asset-account-account-19971_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t GetView `View <type-daml-finance-interface-asset-account-view-6934_>`_
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Account <type-daml-finance-interface-asset-account-account-19971_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t GetView `View <type-daml-finance-interface-asset-account-view-6934_>`_

Functions
---------

.. _function-daml-finance-interface-asset-account-exerciseinterfacebykey-66330:

`exerciseInterfaceByKey <function-daml-finance-interface-asset-account-exerciseinterfacebykey-66330_>`_
  \: `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t2 d r \=\> `K <type-daml-finance-interface-asset-account-k-29547_>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> d \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ r
  
  Exercise interface by key\.
  This method can be used to exercise a choice on an ``Account`` given its ``AccountKey``\.
  Requires as input the ``AccountKey``, the actor fetching the account and the choice arguments\. For example\:
  
  .. code-block:: daml
  
    exerciseInterfaceByKey @Account.I accountKey actor Account.Debit with holdingCid

.. _function-daml-finance-interface-asset-account-tokey-59196:

`toKey <function-daml-finance-interface-asset-account-tokey-59196_>`_
  \: `View <type-daml-finance-interface-asset-account-view-6934_>`_ \-\> :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
  
  Convert the account's 'View' to its key\.

.. _function-daml-finance-interface-asset-account-asdisclosure-47762:

`asDisclosure <function-daml-finance-interface-asset-account-asdisclosure-47762_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Account <type-daml-finance-interface-asset-account-account-19971_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-asset-account-getkey-3746:

`getKey <function-daml-finance-interface-asset-account-getkey-3746_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Account <type-daml-finance-interface-asset-account-account-19971_>`_ \=\> t \-\> :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`

.. _function-daml-finance-interface-asset-account-credit-63464:

`credit <function-daml-finance-interface-asset-account-credit-63464_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Account <type-daml-finance-interface-asset-account-account-19971_>`_ \=\> t \-\> Credit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`)

.. _function-daml-finance-interface-asset-account-debit-87734:

`debit <function-daml-finance-interface-asset-account-debit-87734_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Account <type-daml-finance-interface-asset-account-account-19971_>`_ \=\> t \-\> Debit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
