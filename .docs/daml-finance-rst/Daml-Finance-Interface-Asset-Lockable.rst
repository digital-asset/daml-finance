.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-lockable-43602:

Module Daml.Finance.Interface.Asset.Lockable
============================================

Interfaces
----------

.. _type-daml-finance-interface-asset-lockable-lockable-65857:

**interface** `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_

  An interface respresenting contracts which can restrict a contract by a set of specifed parties
  
  + **Choice Acquire**
    
    Lock a contract\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - newLocker
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Parties which restrain the contracts ability to preform specified actions\.
       * - context
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Reason for acquiring a lock\.
       * - lockType
         - `LockType <type-daml-finance-interface-asset-lockable-locktype-81669_>`_
         - Type of lock to acquire
  
  + **Choice GetView**
    
    Retrieves the instrument view\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.
  
  + **Choice Release**
    
    Unlock a locked contract\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - context
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - 
  
  + **Method acquire \: **Acquire \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_)
    
    Implementation of the ``Acquire`` choice\.
  
  + **Method asHolding \: **:ref:`I <type-daml-finance-interface-asset-holding-i-4221>`
    
    Conversion to ``Holding`` interface\.
  
  + **Method release \: **Release \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_)
    
    Implementation of the ``Release`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-lockable-hasimplementation-62224:

**class** `Implementation <type-daml-finance-interface-asset-lockable-implementation-3140_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-lockable-hasimplementation-62224_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-asset-lockable-i-23182:

**type** `I <type-daml-finance-interface-asset-lockable-i-23182_>`_
  \= `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_
  
  Type synonym for ``Lockable``\.
  
  **instance** HasMethod :ref:`Transferable <type-daml-finance-interface-asset-transferable-transferable-34689>` \"asLockable\" `I <type-daml-finance-interface-asset-lockable-i-23182_>`_

.. _type-daml-finance-interface-asset-lockable-implementation-3140:

**type** `Implementation <type-daml-finance-interface-asset-lockable-implementation-3140_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-asset-lockable-i-23182_>`_, :ref:`Implementation <type-daml-finance-interface-asset-holding-implementation-34045>` t)
  
  Lockable requires Holding\.I

.. _type-daml-finance-interface-asset-lockable-lock-27785:

**data** `Lock <type-daml-finance-interface-asset-lockable-lock-27785_>`_

  Locking details\.
  
  .. _constr-daml-finance-interface-asset-lockable-lock-5616:
  
  `Lock <constr-daml-finance-interface-asset-lockable-lock-5616_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - locker
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Parties which are locking the contract\.
       * - context
         - `Set <https://docs.daml.com/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Why this lock is held by the locking parties\.
       * - lockType
         - `LockType <type-daml-finance-interface-asset-lockable-locktype-81669_>`_
         - The type of lock applied\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Lock <type-daml-finance-interface-asset-lockable-lock-27785_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Lock <type-daml-finance-interface-asset-lockable-lock-27785_>`_

.. _type-daml-finance-interface-asset-lockable-locktype-81669:

**data** `LockType <type-daml-finance-interface-asset-lockable-locktype-81669_>`_

  Type of lock held\.
  
  .. _constr-daml-finance-interface-asset-lockable-semaphore-55910:
  
  `Semaphore <constr-daml-finance-interface-asset-lockable-semaphore-55910_>`_
  
    A one time only lock\.
  
  .. _constr-daml-finance-interface-asset-lockable-reentrant-74803:
  
  `Reentrant <constr-daml-finance-interface-asset-lockable-reentrant-74803_>`_
  
    A mutual exclusion lock where the same lockers may lock a contract multiple times\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `LockType <type-daml-finance-interface-asset-lockable-locktype-81669_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `LockType <type-daml-finance-interface-asset-lockable-locktype-81669_>`_

.. _type-daml-finance-interface-asset-lockable-v-47049:

**type** `V <type-daml-finance-interface-asset-lockable-v-47049_>`_
  \= `View <type-daml-finance-interface-asset-lockable-view-19295_>`_
  
  Type synonym for ``View``\.

.. _type-daml-finance-interface-asset-lockable-view-19295:

**data** `View <type-daml-finance-interface-asset-lockable-view-19295_>`_

  View for ``Lockable``\.
  
  .. _constr-daml-finance-interface-asset-lockable-view-69022:
  
  `View <constr-daml-finance-interface-asset-lockable-view-69022_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - lock
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-asset-lockable-lock-27785_>`_
         - When a contract is locked, contains the locking details\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-lockable-view-19295_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-lockable-view-19295_>`_

Functions
---------

.. _function-daml-finance-interface-asset-lockable-asholding-50506:

`asHolding <function-daml-finance-interface-asset-lockable-asholding-50506_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`

.. _function-daml-finance-interface-asset-lockable-acquire-66725:

`acquire <function-daml-finance-interface-asset-lockable-acquire-66725_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_ \=\> t \-\> Acquire \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_)

.. _function-daml-finance-interface-asset-lockable-release-73338:

`release <function-daml-finance-interface-asset-lockable-release-73338_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_ \=\> t \-\> Release \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-asset-lockable-lockable-65857_>`_)
