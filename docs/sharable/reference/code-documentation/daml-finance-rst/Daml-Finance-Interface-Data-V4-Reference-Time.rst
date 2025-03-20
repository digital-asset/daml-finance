.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-data-v4-reference-time-12465:

Daml.Finance.Interface.Data.V4.Reference.Time
=============================================

This module defines an interface for ``BusinessTime`` rules, which are contracts to control and
keep track of business time\.

Interfaces
----------

.. _type-daml-finance-interface-data-v4-reference-time-time-96632:

**interface** `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_

  An interface to manage and control business time\. Controlled time rules (i\.e\. clocks) are
  managed by entities that have control certain business time events\. These can be trading\-open /
  \-close on an exchange, start\-of\-day / end\-of\-day events of a trading desk, or just a daily
  clock tick to signal the passing of aticking\. Intervals in which
  the clock \"ticks\" don't have to be regular, and can e\.g\. consider business days only\.

  **viewtype** `V <type-daml-finance-interface-data-v4-reference-time-v-96032_>`_

  + .. _type-daml-finance-interface-data-v4-reference-time-advance-64582:

    **Choice** `Advance <type-daml-finance-interface-data-v4-reference-time-advance-64582_>`_

    Advance time to its next state\.

    Controller\: (DA\.Internal\.Record\.getField @\"providers\" (view this))

    Returns\: (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_, `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986>`)

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - eventId
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Event identifier\.
       * - eventDescription
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Event description\.

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-data-v4-reference-time-getview-6375:

    **Choice** `GetView <type-daml-finance-interface-data-v4-reference-time-getview-6375_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-data-v4-reference-time-view-8124_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-data-v4-reference-time-rewind-93104:

    **Choice** `Rewind <type-daml-finance-interface-data-v4-reference-time-rewind-93104_>`_

    Rewind time to its previous state\.

    Controller\: (DA\.Internal\.Record\.getField @\"providers\" (view this))

    Returns\: (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_, `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986>`)

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - eventId
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Event identifier\.
       * - eventDescription
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Event description\.

  + **Method advance \:** `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_ \-\> `Advance <type-daml-finance-interface-data-v4-reference-time-advance-64582_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_, `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986>`)

    Implementation of the ``Advance`` choice\.

  + **Method rewind \:** `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_ \-\> `Rewind <type-daml-finance-interface-data-v4-reference-time-rewind-93104_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_, `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986>`)

    Implementation of the ``Rewind`` choice\.

Data Types
----------

.. _type-daml-finance-interface-data-v4-reference-time-i-2519:

**type** `I <type-daml-finance-interface-data-v4-reference-time-i-2519_>`_
  \= `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_

  Type synonym for ``Time``\.

.. _type-daml-finance-interface-data-v4-reference-time-v-96032:

**type** `V <type-daml-finance-interface-data-v4-reference-time-v-96032_>`_
  \= `View <type-daml-finance-interface-data-v4-reference-time-view-8124_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_ `V <type-daml-finance-interface-data-v4-reference-time-v-96032_>`_

.. _type-daml-finance-interface-data-v4-reference-time-view-8124:

**data** `View <type-daml-finance-interface-data-v4-reference-time-view-8124_>`_

  View for ``Time``\.

  .. _constr-daml-finance-interface-data-v4-reference-time-view-72445:

  `View <constr-daml-finance-interface-data-v4-reference-time-view-72445_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - providers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties controlling time\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Textual identifier for the time rule\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-data-v4-reference-time-view-8124_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-data-v4-reference-time-view-8124_>`_

Functions
---------

.. _function-daml-finance-interface-data-v4-reference-time-advance-63914:

`advance <function-daml-finance-interface-data-v4-reference-time-advance-63914_>`_
  \: `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_ \-\> `Advance <type-daml-finance-interface-data-v4-reference-time-advance-64582_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_, `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986>`)

.. _function-daml-finance-interface-data-v4-reference-time-rewind-25948:

`rewind <function-daml-finance-interface-data-v4-reference-time-rewind-25948_>`_
  \: `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_ \-\> `Rewind <type-daml-finance-interface-data-v4-reference-time-rewind-93104_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Time <type-daml-finance-interface-data-v4-reference-time-time-96632_>`_, `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986>`)
