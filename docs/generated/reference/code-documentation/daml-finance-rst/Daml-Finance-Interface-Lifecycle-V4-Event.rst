.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-event-91777:

Daml.Finance.Interface.Lifecycle.V4.Event
=========================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-event-event-37738:

**interface** `Event <type-daml-finance-interface-lifecycle-v4-event-event-37738_>`_

  A lifecycle event\. These events are ordered based on the corresponding event time\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-event-v-55884_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-event-getview-88787:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-event-getview-88787_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.


Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-event-i-36171:

**type** `I <type-daml-finance-interface-lifecycle-v4-event-i-36171_>`_
  \= `Event <type-daml-finance-interface-lifecycle-v4-event-event-37738_>`_

  Type synonym for ``Event``\.

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"eventCid\" :ref:`Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221>` (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-v4-event-i-36171_>`_)

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"eventCid\" :ref:`Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221>` (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-v4-event-i-36171_>`_)

.. _type-daml-finance-interface-lifecycle-v4-event-v-55884:

**type** `V <type-daml-finance-interface-lifecycle-v4-event-v-55884_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Event <type-daml-finance-interface-lifecycle-v4-event-event-37738_>`_ `V <type-daml-finance-interface-lifecycle-v4-event-v-55884_>`_

.. _type-daml-finance-interface-lifecycle-v4-event-view-53912:

**data** `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_

  View for ``Event``\.

  .. _constr-daml-finance-interface-lifecycle-v4-event-view-99385:

  `View <constr-daml-finance-interface-lifecycle-v4-event-view-99385_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - providers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Providers of the event\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Identifier for the event\.
       * - description
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A human readable description of the event\.
       * - eventTime
         - `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The time of the event\. This allows ordering of events\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"eventTime\" `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"id\" `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"providers\" `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"eventTime\" `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"id\" `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"providers\" `View <type-daml-finance-interface-lifecycle-v4-event-view-53912_>`_ :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`

Functions
---------

.. _function-daml-finance-interface-lifecycle-v4-event-geteventtime-31000:

`getEventTime <function-daml-finance-interface-lifecycle-v4-event-geteventtime-31000_>`_
  \: `Event <type-daml-finance-interface-lifecycle-v4-event-event-37738_>`_ \-\> `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  Given an event, retrieves the event time\.
