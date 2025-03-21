.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-data-v4-numeric-observation-factory-95641:

Daml.Finance.Interface.Data.V4.Numeric.Observation.Factory
==========================================================

Interfaces
----------

.. _type-daml-finance-interface-data-v4-numeric-observation-factory-factory-52430:

**interface** `Factory <type-daml-finance-interface-data-v4-numeric-observation-factory-factory-52430_>`_

  Factory contract used to create, remove and view a ``Numeric.Observation``\.

  **viewtype** `V <type-daml-finance-interface-data-v4-numeric-observation-factory-v-25032_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681:

    **Choice** `Create <type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681_>`_

    Create an ``Observation``\.

    Controller\: provider

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-data-v4-numeric-observation-i-84859>`

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

  + **Method create' \:** `Create <type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-data-v4-numeric-observation-i-84859>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-data-v4-numeric-observation-factory-f-94552:

**type** `F <type-daml-finance-interface-data-v4-numeric-observation-factory-f-94552_>`_
  \= `Factory <type-daml-finance-interface-data-v4-numeric-observation-factory-factory-52430_>`_

.. _type-daml-finance-interface-data-v4-numeric-observation-factory-i-40079:

**type** `I <type-daml-finance-interface-data-v4-numeric-observation-factory-i-40079_>`_
  \= `Factory <type-daml-finance-interface-data-v4-numeric-observation-factory-factory-52430_>`_

  Type synonyms for ``Factory``\.

.. _type-daml-finance-interface-data-v4-numeric-observation-factory-v-25032:

**type** `V <type-daml-finance-interface-data-v4-numeric-observation-factory-v-25032_>`_
  \= `View <type-daml-finance-interface-data-v4-numeric-observation-factory-view-86852_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-data-v4-numeric-observation-factory-factory-52430_>`_ `V <type-daml-finance-interface-data-v4-numeric-observation-factory-v-25032_>`_

.. _type-daml-finance-interface-data-v4-numeric-observation-factory-view-86852:

**data** `View <type-daml-finance-interface-data-v4-numeric-observation-factory-view-86852_>`_

  .. _constr-daml-finance-interface-data-v4-numeric-observation-factory-view-89975:

  `View <constr-daml-finance-interface-data-v4-numeric-observation-factory-view-89975_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The observers of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-data-v4-numeric-observation-factory-view-86852_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-data-v4-numeric-observation-factory-view-86852_>`_

Functions
---------

.. _function-daml-finance-interface-data-v4-numeric-observation-factory-createtick-82983:

`create' <function-daml-finance-interface-data-v4-numeric-observation-factory-createtick-82983_>`_
  \: `Factory <type-daml-finance-interface-data-v4-numeric-observation-factory-factory-52430_>`_ \-\> `Create <type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-data-v4-numeric-observation-i-84859>`)
