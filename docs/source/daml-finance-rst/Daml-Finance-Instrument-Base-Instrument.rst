.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-base-instrument-53549:

Module Daml.Finance.Instrument.Base.Instrument
==============================================

Templates
---------

.. _type-daml-finance-instrument-base-instrument-factory-67142:

**template** `Factory <type-daml-finance-instrument-base-instrument-factory-67142_>`_

  Factory template for instrument creation\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The factory's provider\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - The factory's observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  + **implements** :ref:`F <type-daml-finance-interface-instrument-base-factory-f-82819>`

.. _type-daml-finance-instrument-base-instrument-instrument-9526:

**template** `Instrument <type-daml-finance-instrument-base-instrument-instrument-9526_>`_

  Base implementation for Instrument which does not define any lifecycling logic\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The instrument's depository\.
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The instrument's issuer\.
     * - id
       - :ref:`Id <type-daml-finance-interface-common-types-id-88316>`
       - The intrument's identifier\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
     * - validAsOf
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Timestamp as of which the instrument is valid\. This usually coincides with the timestamp of the event that creates the instrument\. It usually does not coincide with ledger time\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  + **implements** :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-67236>`

Data Types
----------

.. _type-daml-finance-instrument-base-instrument-t-28558:

**type** `T <type-daml-finance-instrument-base-instrument-t-28558_>`_
  \= `Instrument <type-daml-finance-instrument-base-instrument-instrument-9526_>`_
