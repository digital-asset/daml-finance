.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-equity-lifecycle-replacement-53459:

Module Daml.Finance.Instrument.Equity.Lifecycle.Replacement
================================================

Templates
---------

.. _type-daml-finance-instrument-equity-lifecycle-replacement-replacement-69986:

**template** `Replacement <type-daml-finance-instrument-equity-lifecycle-replacement-replacement-69986_>`_

  Replacement of units of an instrument with a basket of other instruments\.

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - offerer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The party offering the takeover\.
     * - executionDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The replacement's execution date\.
     * - targetInstrument
       - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-75164>`
       - The instrument whose units are consumed by the merger\.
     * - offeredAssets
       - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-31714>`\]
       - The instruments whose units are delivered and corresponding amounts\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.

  + **Choice Archive**


  + **implements** :ref:`I <type-daml-finance-interface-instrument-equity-lifecycle-corporateaction-i-47005>`

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-instrument-equity-lifecycle-replacement-t-36756:

**type** `T <type-daml-finance-instrument-equity-lifecycle-replacement-t-36756_>`_
  \= `Replacement <type-daml-finance-instrument-equity-lifecycle-replacement-replacement-69986_>`_

  **instance** :ref:`HasImplementation <class-daml-finance-interface-lifecycle-lifecyclable-hasimplementation-23622>` `T <type-daml-finance-instrument-equity-lifecycle-replacement-t-36756_>`_
