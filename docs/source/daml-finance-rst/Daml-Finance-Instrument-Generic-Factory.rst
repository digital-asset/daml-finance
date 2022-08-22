.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-generic-factory-47575:

Module Daml.Finance.Instrument.Generic.Factory
======================================

Templates
---------

.. _type-daml-finance-instrument-generic-factory-factory-26064:

**template** `Factory <type-daml-finance-instrument-generic-factory-factory-26064_>`_

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

  + **implements** :ref:`F <type-daml-finance-interface-instrument-generic-factory-f-50653>`

Data Types
----------

.. _type-daml-finance-instrument-generic-factory-f-66994:

**type** `F <type-daml-finance-instrument-generic-factory-f-66994_>`_
  \= `Factory <type-daml-finance-instrument-generic-factory-factory-26064_>`_
