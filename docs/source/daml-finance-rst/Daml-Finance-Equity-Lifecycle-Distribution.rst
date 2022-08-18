.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-equity-lifecycle-distribution-18526:

Module Daml.Finance.Equity.Lifecycle.Distribution
=================================================

Templates
---------

.. _type-daml-finance-equity-lifecycle-distribution-distribution-95534:

**template** `Distribution <type-daml-finance-equity-lifecycle-distribution-distribution-95534_>`_

  Distribution of units of an instrument for each unit of a target instrument (e\.g\., share or cash dividends)\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The target instrument's issuer\.
     * - effectiveDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The effective date of the distribution\.
     * - perUnitDistribution
       - \[:ref:`Q <type-daml-finance-interface-asset-instrument-q-31714>`\]
       - Distributed items\.
     * - currentInstrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - Target instrument before the distribution takes place\.
     * - newInstrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - Target instrument after the distribution has taken place\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005>`
  
  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`
  
  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-equity-lifecycle-distribution-t-73379:

**type** `T <type-daml-finance-equity-lifecycle-distribution-t-73379_>`_
  \= `Distribution <type-daml-finance-equity-lifecycle-distribution-distribution-95534_>`_
  
  **instance** :ref:`HasImplementation <class-daml-finance-interface-lifecycle-lifecyclable-hasimplementation-23622>` `T <type-daml-finance-equity-lifecycle-distribution-t-73379_>`_
