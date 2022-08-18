.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-rule-distribution-35531:

Module Daml.Finance.Lifecycle.Rule.Distribution
===============================================

Templates
---------

.. _type-daml-finance-lifecycle-rule-distribution-rule-66267:

**template** `Rule <type-daml-finance-lifecycle-rule-distribution-rule-66267_>`_

  Distribution of units of an instrument for each unit of a target instrument (e\.g\., share or cash dividends)\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Provider of the distribution rule\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers of the distributin rule\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-lifecycle-rule-distribution-t-91388:

**type** `T <type-daml-finance-lifecycle-rule-distribution-t-91388_>`_
  \= `Rule <type-daml-finance-lifecycle-rule-distribution-rule-66267_>`_
