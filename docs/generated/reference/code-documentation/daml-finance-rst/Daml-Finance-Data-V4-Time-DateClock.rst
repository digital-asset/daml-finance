.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-data-v4-time-dateclock-1389:

Daml.Finance.Data.V4.Time.DateClock
===================================

Templates
---------

.. _type-daml-finance-data-v4-time-dateclock-dateclock-18944:

**template** `DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944_>`_

  A ``DateClock`` is a template used to keep track of the current date\.
  It implements the ``Time`` rule interface to be able to advance and rewind business time\. It also
  implements the ``TimeObservable`` interface\. Specifically, each date ``D`` is mapped to
  ``D 00:00:00 UTC``\. If your use\-case involves working across multiple time zones, you may need to
  define multiple ``DateClock`` templates with specific time conversions\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - The clock's providers\.
     * - date
       - :ref:`Unit <type-daml-finance-data-v4-time-dateclock-types-unit-49992>`
       - The clock's date\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - The clock's identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The clock's description\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Observers\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-data-v4-reference-time-i-2519>` **for** `DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-observable-timeobservable-i-1376>` **for** `DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944_>`_

Data Types
----------

.. _type-daml-finance-data-v4-time-dateclock-t-22118:

**type** `T <type-daml-finance-data-v4-time-dateclock-t-22118_>`_
  \= `DateClock <type-daml-finance-data-v4-time-dateclock-dateclock-18944_>`_

  Type synonym for ``DateClock``\.

Functions
---------

.. _function-daml-finance-data-v4-time-dateclock-datetodateclocktime-8972:

`dateToDateClockTime <function-daml-finance-data-v4-time-dateclock-datetodateclocktime-8972_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  Maps a ``Date`` to ``Time`` using the rule in the ``DateClock``\.
