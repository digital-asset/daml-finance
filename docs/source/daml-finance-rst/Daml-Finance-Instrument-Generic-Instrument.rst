.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-generic-instrument-74885:

Module Daml.Finance.Instrument.Generic.Instrument
=========================================

Templates
---------

.. _type-daml-finance-instrument-generic-instrument-instrument-92650:

**template** `Instrument <type-daml-finance-instrument-generic-instrument-instrument-92650_>`_

  An instrument representing a generic derivative, modelled using the Contingent Claims library\.
  The responsibility for processing lifecycle events as well as elections is delegated to the issuer, who is hence responsible for providing the correct ``Observable``\\s\.

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The instrument depository\.
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The instrument issuer\.
     * - id
       - :ref:`Id <type-daml-finance-interface-asset-types-id-89116>`
       - A textual identifier\.
     * - claims
       - :ref:`C <type-daml-finance-interface-instrument-generic-types-c-63687>`
       - The claim tree\.
     * - acquisitionTime
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - The claim's acquisition time\. This usually corresponds to the start date of the contract\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  + **Choice Archive**


  + **implements** :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

  + **implements** :ref:`Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012>`

  + **implements** :ref:`I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893>`

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-instrument-generic-instrument-t-62954:

**type** `T <type-daml-finance-instrument-generic-instrument-t-62954_>`_
  \= `Instrument <type-daml-finance-instrument-generic-instrument-instrument-92650_>`_

  **instance** :ref:`HasImplementation <class-daml-finance-interface-asset-instrument-hasimplementation-51108>` `T <type-daml-finance-instrument-generic-instrument-t-62954_>`_

  **instance** :ref:`ExercisableHasImplementation <class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-7032>` `T <type-daml-finance-instrument-generic-instrument-t-62954_>`_
