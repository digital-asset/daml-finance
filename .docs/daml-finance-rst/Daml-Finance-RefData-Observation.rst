.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-refdata-observation-94498:

Module Daml.Finance.RefData.Observation
=======================================

Templates
---------

.. _type-daml-finance-refdata-observation-observation-39199:

**template** `Observation <type-daml-finance-refdata-observation-observation-39199_>`_

  An implementation of ``Observable`` that explicitly stores time\-dependent numerical values\.
  It can be used for e\.g\. equity or rate fixings\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The reference data provider\.
     * - obsKey
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - observations
       - `Map <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The time\-dependent values\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    
