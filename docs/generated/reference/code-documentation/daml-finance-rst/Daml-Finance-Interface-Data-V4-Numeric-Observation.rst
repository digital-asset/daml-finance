.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-data-v4-numeric-observation-41921:

Daml.Finance.Interface.Data.V4.Numeric.Observation
==================================================

Interfaces
----------

.. _type-daml-finance-interface-data-v4-numeric-observation-observation-69182:

**interface** `Observation <type-daml-finance-interface-data-v4-numeric-observation-observation-69182_>`_

  Interface for a time\-dependent numeric ``Observation``, where the values are explicitly stored
  on\-ledger\.

  **viewtype** `V <type-daml-finance-interface-data-v4-numeric-observation-v-43612_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-data-v4-numeric-observation-getview-73155:

    **Choice** `GetView <type-daml-finance-interface-data-v4-numeric-observation-getview-73155_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-data-v4-numeric-observation-view-99464_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party fetching the view\.


Data Types
----------

.. _type-daml-finance-interface-data-v4-numeric-observation-i-84859:

**type** `I <type-daml-finance-interface-data-v4-numeric-observation-i-84859_>`_
  \= `Observation <type-daml-finance-interface-data-v4-numeric-observation-observation-69182_>`_

  Type synonym for ``Observation``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-data-v4-numeric-observation-factory-factory-52430>` \"create'\" (:ref:`Create <type-daml-finance-interface-data-v4-numeric-observation-factory-create-1681>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-data-v4-numeric-observation-i-84859_>`_))

.. _type-daml-finance-interface-data-v4-numeric-observation-v-43612:

**type** `V <type-daml-finance-interface-data-v4-numeric-observation-v-43612_>`_
  \= `View <type-daml-finance-interface-data-v4-numeric-observation-view-99464_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Observation <type-daml-finance-interface-data-v4-numeric-observation-observation-69182_>`_ `V <type-daml-finance-interface-data-v4-numeric-observation-v-43612_>`_

.. _type-daml-finance-interface-data-v4-numeric-observation-view-99464:

**data** `View <type-daml-finance-interface-data-v4-numeric-observation-view-99464_>`_

  View for ``Observation``\.

  .. _constr-daml-finance-interface-data-v4-numeric-observation-view-52415:

  `View <constr-daml-finance-interface-data-v4-numeric-observation-view-52415_>`_

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
