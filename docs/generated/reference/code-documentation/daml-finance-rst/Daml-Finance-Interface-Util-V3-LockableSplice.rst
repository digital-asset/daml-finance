.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-util-v3-lockablesplice-95881:

Daml.Finance.Interface.Util.V3.LockableSplice
=============================================

Interfaces
----------

.. _type-daml-finance-interface-util-v3-lockablesplice-lockable-7998:

**interface** `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_

  Lockable interface, layered on top of Splice Holding\.
  This allows tests that used to lock Daml\.Finance holdings
  to still work against Splice holdings\.

  **viewtype** `V <type-daml-finance-interface-util-v3-lockablesplice-v-15356_>`_

  + .. _type-daml-finance-interface-util-v3-lockablesplice-acquire-89216:

    **Choice** `Acquire <type-daml-finance-interface-util-v3-lockablesplice-acquire-89216_>`_

    Lock a contract\.

    Controller\: (DA\.Internal\.Record\.getField @\"controllers\" (view this)), newLockers

    Returns\: `ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_

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
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Reason for acquiring a lock\.
       * - lockType
         - `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_
         - Type of lock to acquire\.

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-util-v3-lockablesplice-getview-16067:

    **Choice** `GetView <type-daml-finance-interface-util-v3-lockablesplice-getview-16067_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-util-v3-lockablesplice-v-15356_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-util-v3-lockablesplice-release-17663:

    **Choice** `Release <type-daml-finance-interface-util-v3-lockablesplice-release-17663_>`_

    Unlock a locked contract\.

    Controller\: getLockers this

    Returns\: `ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - context
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -

  + **Method acquire \:** `Acquire <type-daml-finance-interface-util-v3-lockablesplice-acquire-89216_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_)

    Implementation of the ``Acquire`` choice\.

  + **Method release \:** `Release <type-daml-finance-interface-util-v3-lockablesplice-release-17663_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_)

    Implementation of the ``Release`` choice\.

Data Types
----------

.. _type-daml-finance-interface-util-v3-lockablesplice-i-56603:

**type** `I <type-daml-finance-interface-util-v3-lockablesplice-i-56603_>`_
  \= `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_

  Type synonym for ``Lockable``\.

.. _type-daml-finance-interface-util-v3-lockablesplice-lock-9150:

**data** `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_

  Locking details\.

  .. _constr-daml-finance-interface-util-v3-lockablesplice-lock-23007:

  `Lock <constr-daml-finance-interface-util-v3-lockablesplice-lock-23007_>`_

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
         - `Set <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Context of the lock, explaining why this lock is held by the locking parties\. If the lock is reentrant, there may be more than one locking context for the set of lockers\.
       * - lockType
         - `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_
         - The type of lock applied\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"context\" `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_ (`Set <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lock\" :ref:`Account <type-daml-finance-account-v4-account-account-35720>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lock\" :ref:`BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lock\" :ref:`Fungible <type-daml-finance-holding-v4-fungible-fungible-67336>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lock\" :ref:`Transferable <type-daml-finance-holding-v4-transferable-transferable-12222>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lock\" :ref:`TransferableFungible <type-daml-finance-holding-v4-transferablefungible-transferablefungible-50906>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lock\" `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lockType\" `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_ `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lockers\" `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"context\" `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_ (`Set <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lock\" :ref:`Account <type-daml-finance-account-v4-account-account-35720>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lock\" :ref:`BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lock\" :ref:`Fungible <type-daml-finance-holding-v4-fungible-fungible-67336>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lock\" :ref:`Transferable <type-daml-finance-holding-v4-transferable-transferable-12222>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lock\" :ref:`TransferableFungible <type-daml-finance-holding-v4-transferablefungible-transferablefungible-50906>` (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lock\" `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lockType\" `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_ `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lockers\" `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

.. _type-daml-finance-interface-util-v3-lockablesplice-locktype-5010:

**data** `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

  Type of lock held\.

  .. _constr-daml-finance-interface-util-v3-lockablesplice-semaphore-71091:

  `Semaphore <constr-daml-finance-interface-util-v3-lockablesplice-semaphore-71091_>`_

    A one time only lock\.

  .. _constr-daml-finance-interface-util-v3-lockablesplice-reentrant-17802:

  `Reentrant <constr-daml-finance-interface-util-v3-lockablesplice-reentrant-17802_>`_

    A mutual exclusion lock where the same lockers may lock a contract multiple times\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lockType\" `Acquire <type-daml-finance-interface-util-v3-lockablesplice-acquire-89216_>`_ `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lockType\" `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_ `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lockType\" `Acquire <type-daml-finance-interface-util-v3-lockablesplice-acquire-89216_>`_ `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lockType\" `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_ `LockType <type-daml-finance-interface-util-v3-lockablesplice-locktype-5010_>`_

.. _type-daml-finance-interface-util-v3-lockablesplice-v-15356:

**type** `V <type-daml-finance-interface-util-v3-lockablesplice-v-15356_>`_
  \= `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_ `V <type-daml-finance-interface-util-v3-lockablesplice-v-15356_>`_

.. _type-daml-finance-interface-util-v3-lockablesplice-view-30376:

**data** `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_

  View for ``Lockable``\.

  .. _constr-daml-finance-interface-util-v3-lockablesplice-view-93209:

  `View <constr-daml-finance-interface-util-v3-lockablesplice-view-93209_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lock
         - `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_
         - An optional lock, indicating if it is locked or not\.
       * - controllers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - All parties whose authorization is required to acquire a lock\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"controllers\" `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lock\" `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"controllers\" `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lock\" `View <type-daml-finance-interface-util-v3-lockablesplice-view-30376_>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150_>`_)

Functions
---------

.. _function-daml-finance-interface-util-v3-lockablesplice-acquire-4100:

`acquire <function-daml-finance-interface-util-v3-lockablesplice-acquire-4100_>`_
  \: `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_ \-\> `Acquire <type-daml-finance-interface-util-v3-lockablesplice-acquire-89216_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_)

.. _function-daml-finance-interface-util-v3-lockablesplice-release-37667:

`release <function-daml-finance-interface-util-v3-lockablesplice-release-37667_>`_
  \: `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_ \-\> `Release <type-daml-finance-interface-util-v3-lockablesplice-release-17663_>`_ \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_)

.. _function-daml-finance-interface-util-v3-lockablesplice-getlockers-53786:

`getLockers <function-daml-finance-interface-util-v3-lockablesplice-getlockers-53786_>`_
  \: `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_ \=\> t \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  Retrieves the lockers of a ``Lockable``\.

.. _function-daml-finance-interface-util-v3-lockablesplice-mustnotbelocked-97679:

`mustNotBeLocked <function-daml-finance-interface-util-v3-lockablesplice-mustnotbelocked-97679_>`_
  \: `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i `Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998_>`_ \=\> i \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

  Asserts that a lockable is not locked\.
