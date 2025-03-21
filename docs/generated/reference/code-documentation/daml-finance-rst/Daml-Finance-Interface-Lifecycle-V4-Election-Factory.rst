.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-election-factory-21763:

Daml.Finance.Interface.Lifecycle.V4.Election.Factory
====================================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-election-factory-factory-95552:

**interface** `Factory <type-daml-finance-interface-lifecycle-v4-election-factory-factory-95552_>`_

  Factory interface to instantiate elections on generic instruments\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-election-factory-v-45490_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-election-factory-create-20391:

    **Choice** `Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391_>`_

    Create a new Election\.

    Controller\: actors

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-election-i-36777>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties calling the ``Create`` choice\.
       * - elector
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Parties on behalf of which the election is made\.
       * - counterparty
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Faces the ``elector`` in the ``Holding``\.
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party that signs the election (together with the elector)\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - The identifier for an election\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A description of the instrument\.
       * - claim
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The tag corresponding to the elected sub\-tree\.
       * - electorIsOwner
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - ``True`` if election is on behalf of the owner of the holding, ``False`` otherwise\.
       * - electionTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Time at which the election is put forward\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - Observers of the election\.
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Number of instrument units to which the election applies\.
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument to which the election applies\.

  + **Method create' \:** `Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-election-i-36777>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-election-factory-i-34597:

**type** `I <type-daml-finance-interface-lifecycle-v4-election-factory-i-34597_>`_
  \= `Factory <type-daml-finance-interface-lifecycle-v4-election-factory-factory-95552_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-lifecycle-v4-election-factory-v-45490:

**type** `V <type-daml-finance-interface-lifecycle-v4-election-factory-v-45490_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-election-factory-view-55502_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-lifecycle-v4-election-factory-factory-95552_>`_ `V <type-daml-finance-interface-lifecycle-v4-election-factory-v-45490_>`_

.. _type-daml-finance-interface-lifecycle-v4-election-factory-view-55502:

**data** `View <type-daml-finance-interface-lifecycle-v4-election-factory-view-55502_>`_

  .. _constr-daml-finance-interface-lifecycle-v4-election-factory-view-88681:

  `View <constr-daml-finance-interface-lifecycle-v4-election-factory-view-88681_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-election-factory-view-55502_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-election-factory-view-55502_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-v4-election-factory-createtick-34237:

`create' <function-daml-finance-interface-lifecycle-v4-election-factory-createtick-34237_>`_
  \: `Factory <type-daml-finance-interface-lifecycle-v4-election-factory-factory-95552_>`_ \-\> `Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-election-i-36777>`)
