.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-data-v4-numeric-observation-19522:

Daml.Finance.Data.V4.Numeric.Observation
========================================

Templates
---------

.. _type-daml-finance-data-v4-numeric-observation-factory-28223:

**template** `Factory <type-daml-finance-data-v4-numeric-observation-factory-28223_>`_

  Implementation of the corresponding Observation Factory\.

  Signatory\: provider

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
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The factory's observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-data-v4-numeric-observation-factory-i-40079>` **for** `Factory <type-daml-finance-data-v4-numeric-observation-factory-28223_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-data-v4-numeric-observation-factory-28223_>`_

.. _type-daml-finance-data-v4-numeric-observation-observation-13815:

**template** `Observation <type-daml-finance-data-v4-numeric-observation-observation-13815_>`_

  An implementation of ``NumericObservable`` that explicitly stores time\-dependent numerical
  values\. For example, it can be used for equity or rate fixings\.

  Signatory\: provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The reference data provider\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - A textual identifier\.
     * - observations
       - `Map <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The time\-dependent values\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-data-v4-numeric-observation-i-84859>` **for** `Observation <type-daml-finance-data-v4-numeric-observation-observation-13815_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-i-61855>` **for** `Observation <type-daml-finance-data-v4-numeric-observation-observation-13815_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Observation <type-daml-finance-data-v4-numeric-observation-observation-13815_>`_

Data Types
----------

.. _type-daml-finance-data-v4-numeric-observation-t-41827:

**type** `T <type-daml-finance-data-v4-numeric-observation-t-41827_>`_
  \= `Observation <type-daml-finance-data-v4-numeric-observation-observation-13815_>`_

  Type synonym for ``Observation``\.
