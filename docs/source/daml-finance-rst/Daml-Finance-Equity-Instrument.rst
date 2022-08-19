.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-equity-instrument-16487:

Module Daml.Finance.Equity.Instrument
=====================================

Templates
---------

.. _type-daml-finance-equity-instrument-instrument-7660:

**template** `Instrument <type-daml-finance-equity-instrument-instrument-7660_>`_

  An Instrument representing a common stock\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Issuer\.
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Depository\.
     * - id
       - :ref:`Id <type-daml-finance-interface-asset-types-id-89116>`
       - A textual identifier\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
     * - validAsOf
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Timestamp as of which the instrument is valid\. This usually coincides with the timestamp of the event that creates the instrument\. It usually does not coincide with ledger time\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`
  
  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  + **implements** :ref:`I <type-daml-finance-interface-equity-instrument-i-54484>`

Data Types
----------

.. _type-daml-finance-equity-instrument-t-33420:

**type** `T <type-daml-finance-equity-instrument-t-33420_>`_
  \= `Instrument <type-daml-finance-equity-instrument-instrument-7660_>`_
