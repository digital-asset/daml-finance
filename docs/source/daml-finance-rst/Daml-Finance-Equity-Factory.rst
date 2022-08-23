.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-equity-factory-381:

Module Daml.Finance.Equity.Factory
==================================

Templates
---------

.. _type-daml-finance-equity-factory-factory-66762:

**template** `Factory <type-daml-finance-equity-factory-factory-66762_>`_

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
  
  + **implements** :ref:`F <type-daml-finance-interface-equity-factory-f-7879>`

Data Types
----------

.. _type-daml-finance-equity-factory-f-31836:

**type** `F <type-daml-finance-equity-factory-f-31836_>`_
  \= `Factory <type-daml-finance-equity-factory-factory-66762_>`_
