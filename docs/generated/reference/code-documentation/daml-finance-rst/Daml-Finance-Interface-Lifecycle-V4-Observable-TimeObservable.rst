.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-observable-timeobservable-64296:

Daml.Finance.Interface.Lifecycle.V4.Observable.TimeObservable
=============================================================

This module defines an interface for a ``TimeObservable``, which is implemented by templates
exposing time information\.

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-observable-timeobservable-timeobservable-60264:

**interface** `TimeObservable <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-timeobservable-60264_>`_

  An interface to inspect a time value\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-v-7863_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-observable-timeobservable-gettime-64432:

    **Choice** `GetTime <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-gettime-64432_>`_

    Retrieves the current time\.

    Controller\: actors

    Returns\: `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The party retrieving the current time\.

  + .. _type-daml-finance-interface-lifecycle-v4-observable-timeobservable-getview-28136:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-getview-28136_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method getTime \:** `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

    Implementation of the ``GetTime`` choice\.

Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-observable-timeobservable-i-1376:

**type** `I <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-i-1376_>`_
  \= `TimeObservable <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-timeobservable-60264_>`_

  Type synonym for ``TimeObservable``\.

.. _type-daml-finance-interface-lifecycle-v4-observable-timeobservable-v-7863:

**type** `V <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-v-7863_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `TimeObservable <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-timeobservable-60264_>`_ `V <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-v-7863_>`_

.. _type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477:

**data** `View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477_>`_

  View for ``TimeObservable``\.

  .. _constr-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-24772:

  `View <constr-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-24772_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - providers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties providing the observation\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Textual reference to the observable\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-view-74477_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-v4-observable-timeobservable-gettime-11676:

`getTime <function-daml-finance-interface-lifecycle-v4-observable-timeobservable-gettime-11676_>`_
  \: `TimeObservable <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-timeobservable-60264_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
