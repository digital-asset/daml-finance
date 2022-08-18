.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-rule-replacement-6984:

Module Daml.Finance.Lifecycle.Rule.Replacement
==============================================

Templates
---------

.. _type-daml-finance-lifecycle-rule-replacement-rule-7648:

**template** `Rule <type-daml-finance-lifecycle-rule-replacement-rule-7648_>`_

  Replacement of units of an instrument with a basket of other instruments\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Provider of the replacement rule\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-lifecycle-rule-replacement-t-34745:

**type** `T <type-daml-finance-lifecycle-rule-replacement-t-34745_>`_
  \= `Rule <type-daml-finance-lifecycle-rule-replacement-rule-7648_>`_
