.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-util-v3-lockable-20339:

Daml.Finance.Interface.Util.V3.Lockable
=======================================

Interfaces
----------

.. _type-daml-finance-interface-util-v3-lockable-lockable-79556:

**interface** `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_

  An interface for managing locking of contracts\. Locking is a mechanism to temporarily encumber
  contracts by adding third\-party lockers as additional signatories\.

  **viewtype** `V <type-daml-finance-interface-util-v3-lockable-v-6042_>`_

  + .. _type-daml-finance-interface-util-v3-lockable-acquire-20270:

    **Choice** `Acquire <type-daml-finance-interface-util-v3-lockable-acquire-20270_>`_

    Lock a contract\.

    Controller\: (DA\.Internal\.Record\.getField @\"controllers\" (view this)), newLockers

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - newLockers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties which restrain the contract's ability to perform specified actions\.
       * - context
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Reason for acquiring a lock\.
       * - lockType
         - `LockType <type-daml-finance-interface-util-v3-lockable-locktype-58900_>`_
         - Type of lock to acquire\.

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-util-v3-lockable-getview-61317:

    **Choice** `GetView <type-daml-finance-interface-util-v3-lockable-getview-61317_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-util-v3-lockable-v-6042_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-util-v3-lockable-release-15493:

    **Choice** `Release <type-daml-finance-interface-util-v3-lockable-release-15493_>`_

    Unlock a locked contract\.

    Controller\: getLockers this

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - context
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -

  + **Method acquire \:** `Acquire <type-daml-finance-interface-util-v3-lockable-acquire-20270_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_)

    Implementation of the ``Acquire`` choice\.

  + **Method release \:** `Release <type-daml-finance-interface-util-v3-lockable-release-15493_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_)

    Implementation of the ``Release`` choice\.

Data Types
----------

.. _type-daml-finance-interface-util-v3-lockable-i-3709:

**type** `I <type-daml-finance-interface-util-v3-lockable-i-3709_>`_
  \= `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_

  Type synonym for ``Lockable``\.

.. _type-daml-finance-interface-util-v3-lockable-lock-18728:

**data** `Lock <type-daml-finance-interface-util-v3-lockable-lock-18728_>`_

  Locking details\.

  .. _constr-daml-finance-interface-util-v3-lockable-lock-39673:

  `Lock <constr-daml-finance-interface-util-v3-lockable-lock-39673_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lockers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties which are locking the contract\.
       * - context
         - `Set <https://docs.daml.com/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Context of the lock, explaining why this lock is held by the locking parties\. If the lock is reentrant, there may be more than one locking context for the set of lockers\.
       * - lockType
         - `LockType <type-daml-finance-interface-util-v3-lockable-locktype-58900_>`_
         - The type of lock applied\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Lock <type-daml-finance-interface-util-v3-lockable-lock-18728_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Lock <type-daml-finance-interface-util-v3-lockable-lock-18728_>`_

.. _type-daml-finance-interface-util-v3-lockable-locktype-58900:

**data** `LockType <type-daml-finance-interface-util-v3-lockable-locktype-58900_>`_

  Type of lock held\.

  .. _constr-daml-finance-interface-util-v3-lockable-semaphore-62185:

  `Semaphore <constr-daml-finance-interface-util-v3-lockable-semaphore-62185_>`_

    A one time only lock\.

  .. _constr-daml-finance-interface-util-v3-lockable-reentrant-91604:

  `Reentrant <constr-daml-finance-interface-util-v3-lockable-reentrant-91604_>`_

    A mutual exclusion lock where the same lockers may lock a contract multiple times\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `LockType <type-daml-finance-interface-util-v3-lockable-locktype-58900_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `LockType <type-daml-finance-interface-util-v3-lockable-locktype-58900_>`_

.. _type-daml-finance-interface-util-v3-lockable-v-6042:

**type** `V <type-daml-finance-interface-util-v3-lockable-v-6042_>`_
  \= `View <type-daml-finance-interface-util-v3-lockable-view-77974_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_ `V <type-daml-finance-interface-util-v3-lockable-v-6042_>`_

.. _type-daml-finance-interface-util-v3-lockable-view-77974:

**data** `View <type-daml-finance-interface-util-v3-lockable-view-77974_>`_

  View for ``Lockable``\.

  .. _constr-daml-finance-interface-util-v3-lockable-view-74191:

  `View <constr-daml-finance-interface-util-v3-lockable-view-74191_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lock
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockable-lock-18728_>`_
         - An optional lock, indicating if it is locked or not\.
       * - controllers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - All parties whose authorization is required to acquire a lock\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-util-v3-lockable-view-77974_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-util-v3-lockable-view-77974_>`_

Functions
---------

.. _function-daml-finance-interface-util-v3-lockable-acquire-83170:

`acquire <function-daml-finance-interface-util-v3-lockable-acquire-83170_>`_
  \: `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_ \-\> `Acquire <type-daml-finance-interface-util-v3-lockable-acquire-20270_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_)

.. _function-daml-finance-interface-util-v3-lockable-release-93513:

`release <function-daml-finance-interface-util-v3-lockable-release-93513_>`_
  \: `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_ \-\> `Release <type-daml-finance-interface-util-v3-lockable-release-15493_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_)

.. _function-daml-finance-interface-util-v3-lockable-getlockers-25940:

`getLockers <function-daml-finance-interface-util-v3-lockable-getlockers-25940_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_ \=\> t \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  Retrieves the lockers of a ``Lockable``\.

.. _function-daml-finance-interface-util-v3-lockable-mustnotbelocked-7241:

`mustNotBeLocked <function-daml-finance-interface-util-v3-lockable-mustnotbelocked-7241_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i `Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556_>`_ \=\> i \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

  Asserts that a lockable is not locked\.
