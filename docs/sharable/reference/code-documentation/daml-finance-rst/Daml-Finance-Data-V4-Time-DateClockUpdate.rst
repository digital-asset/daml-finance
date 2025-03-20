.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-data-v4-time-dateclockupdate-59678:

Daml.Finance.Data.V4.Time.DateClockUpdate
=========================================

Templates
---------

.. _type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083:

**template** `DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083_>`_

  Event signalling the update of a ``DateClock``\. It can trigger the execution of lifecycle rules
  for some instruments\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Providers of the event\.
     * - date
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The updated clock data\.
     * - eventTime
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - The event time\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Event identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Event description\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - The clock's observers\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>` **for** `DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-event-time-i-76491>` **for** `DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083_>`_

Data Types
----------

.. _type-daml-finance-data-v4-time-dateclockupdate-t-51075:

**type** `T <type-daml-finance-data-v4-time-dateclockupdate-t-51075_>`_
  \= `DateClockUpdateEvent <type-daml-finance-data-v4-time-dateclockupdate-dateclockupdateevent-31083_>`_

  Type synonym for ``DateClockUpdateEvent``\.
