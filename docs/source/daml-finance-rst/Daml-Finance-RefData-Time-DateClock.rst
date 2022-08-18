.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-refdata-time-dateclock-80226:

Module Daml.Finance.RefData.Time.DateClock
==========================================

Templates
---------

.. _type-daml-finance-refdata-time-dateclock-dateclock-68517:

**template** `DateClock <type-daml-finance-refdata-time-dateclock-dateclock-68517_>`_

  A clock where time is discretized into dates\. Each date is mapped to UTC noon\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - u
       - `Unit <type-daml-finance-refdata-time-dateclock-unit-39282_>`_
       - The clock's date\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The clock's provider\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - The clock's observers\.
  
  + **Choice Archive**
    
  
  + **Choice ToNext**
    
    Moves the clock to the next date and spawns an update event\.
    

.. _type-daml-finance-refdata-time-dateclock-dateclockupdateevent-11763:

**template** `DateClockUpdateEvent <type-daml-finance-refdata-time-dateclock-dateclockupdateevent-11763_>`_

  Event signalling the update of a clock\. It can trigger the execution of lifecycle rules for some instruments\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - clock
       - `DateClock <type-daml-finance-refdata-time-dateclock-dateclock-68517_>`_
       - The updated clock data\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
  
  + **Choice Archive**
    

Data Types
----------

.. _type-daml-finance-refdata-time-dateclock-unit-39282:

**data** `Unit <type-daml-finance-refdata-time-dateclock-unit-39282_>`_

  A ``Date`` which admits a convertion to ``Time`` (it is converted to UTC noon)\.
  
  .. _constr-daml-finance-refdata-time-dateclock-unit-42259:
  
  `Unit <constr-daml-finance-refdata-time-dateclock-unit-42259_>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  
  **instance** :ref:`HasUTCTimeConversion <class-daml-finance-interface-common-classes-hasutctimeconversion-72400>` `Unit <type-daml-finance-refdata-time-dateclock-unit-39282_>`_
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Unit <type-daml-finance-refdata-time-dateclock-unit-39282_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `Unit <type-daml-finance-refdata-time-dateclock-unit-39282_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Unit <type-daml-finance-refdata-time-dateclock-unit-39282_>`_
