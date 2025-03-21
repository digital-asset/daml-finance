.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-observable-numericobservable-50817:

Daml.Finance.Interface.Lifecycle.V4.Observable.NumericObservable
================================================================

This module defines an interface for a ``NumericObservable``, which is used to inspect
time\-dependent numerical values\.

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-observable-numericobservable-numericobservable-46728:

**interface** `NumericObservable <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-numericobservable-46728_>`_

  An interface to inspect some (time\-dependent) numerical values (e\.g\. a stock price or an
  interest rate fixing) required when processing a lifecycle rule\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-v-85848_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-observable-numericobservable-getview-36559:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-getview-36559_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-29492_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-lifecycle-v4-observable-numericobservable-observe-90440:

    **Choice** `Observe <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-observe-90440_>`_

    Observe the ``Observable``\.

    Controller\: actors

    Returns\: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties calling this 'Observe' choice\.
       * - t
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Time at which the value is observed\.

  + **Method observe \:** `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

    Implementation of the ``Observe`` choice\.

Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-observable-numericobservable-i-61855:

**type** `I <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-i-61855_>`_
  \= `NumericObservable <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-numericobservable-46728_>`_

  Type synonym for ``Observable``\.

.. _type-daml-finance-interface-lifecycle-v4-observable-numericobservable-v-85848:

**type** `V <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-v-85848_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-29492_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `NumericObservable <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-numericobservable-46728_>`_ `V <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-v-85848_>`_

.. _type-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-29492:

**data** `View <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-29492_>`_

  View for ``Observable``\.

  .. _constr-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-33363:

  `View <constr-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-33363_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing the observations\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Textual reference to the observable\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-29492_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-view-29492_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-v4-observable-numericobservable-observe-58380:

`observe <function-daml-finance-interface-lifecycle-v4-observable-numericobservable-observe-58380_>`_
  \: `NumericObservable <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-numericobservable-46728_>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
