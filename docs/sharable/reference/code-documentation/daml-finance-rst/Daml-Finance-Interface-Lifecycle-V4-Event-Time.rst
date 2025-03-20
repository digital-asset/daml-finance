.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-event-time-69757:

Daml.Finance.Interface.Lifecycle.V4.Event.Time
==============================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-event-time-event-26986:

**interface** `Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986_>`_

  Event interface for events that signal the passing of (business) time\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-event-time-v-96204_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-event-time-getview-89523:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-event-time-getview-89523_>`_

    Retrieves the event view\. The event's time can be retrieved from the generic ``Event``
    interface\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-event-time-view-67384_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method advance \:** `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Time <type-daml-finance-interface-data-v4-reference-time-time-96632>` \-\> :ref:`Advance <type-daml-finance-interface-data-v4-reference-time-advance-64582>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Time <type-daml-finance-interface-data-v4-reference-time-time-96632>`, `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986_>`_)

    Implementation of the ``Advance`` choice\.

  + **Method rewind \:** `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Time <type-daml-finance-interface-data-v4-reference-time-time-96632>` \-\> :ref:`Rewind <type-daml-finance-interface-data-v4-reference-time-rewind-93104>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Time <type-daml-finance-interface-data-v4-reference-time-time-96632>`, `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986_>`_)

    Implementation of the ``Rewind`` choice\.

Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-event-time-i-76491:

**type** `I <type-daml-finance-interface-lifecycle-v4-event-time-i-76491_>`_
  \= `Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986_>`_

  Type synonym for ``Event``\.

.. _type-daml-finance-interface-lifecycle-v4-event-time-v-96204:

**type** `V <type-daml-finance-interface-lifecycle-v4-event-time-v-96204_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-event-time-view-67384_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Event <type-daml-finance-interface-lifecycle-v4-event-time-event-26986_>`_ `V <type-daml-finance-interface-lifecycle-v4-event-time-v-96204_>`_

.. _type-daml-finance-interface-lifecycle-v4-event-time-view-67384:

**data** `View <type-daml-finance-interface-lifecycle-v4-event-time-view-67384_>`_

  View for ``Event``\.

  .. _constr-daml-finance-interface-lifecycle-v4-event-time-view-61151:

  `View <constr-daml-finance-interface-lifecycle-v4-event-time-view-61151_>`_

    (no fields)

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-event-time-view-67384_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-event-time-view-67384_>`_
