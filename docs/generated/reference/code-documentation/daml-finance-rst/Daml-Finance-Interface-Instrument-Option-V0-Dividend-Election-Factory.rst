.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-dividend-election-factory-93665:

Daml.Finance.Interface.Instrument.Option.V0.Dividend.Election.Factory
=====================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-option-v0-dividend-election-factory-factory-44378:

**interface** `Factory <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-factory-44378_>`_

  Factory interface to instantiate elections on generic instruments\.

  **viewtype** `V <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-v-32700_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397:

    **Choice** `Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397_>`_

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
       * - claimType
         - :ref:`ElectionTypeEnum <type-daml-finance-interface-instrument-option-v0-dividend-types-electiontypeenum-74590>`
         - The election type corresponding to the elected sub\-tree\.
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

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-election-i-36777>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-dividend-election-factory-i-73947:

**type** `I <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-i-73947_>`_
  \= `Factory <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-factory-44378_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-option-v0-dividend-election-factory-v-32700:

**type** `V <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-v-32700_>`_
  \= `View <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-view-99272_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-factory-44378_>`_ `V <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-v-32700_>`_

.. _type-daml-finance-interface-instrument-option-v0-dividend-election-factory-view-99272:

**data** `View <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-view-99272_>`_

  .. _constr-daml-finance-interface-instrument-option-v0-dividend-election-factory-view-87893:

  `View <constr-daml-finance-interface-instrument-option-v0-dividend-election-factory-view-87893_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-view-99272_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-view-99272_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-option-v0-dividend-election-factory-createtick-371:

`create' <function-daml-finance-interface-instrument-option-v0-dividend-election-factory-createtick-371_>`_
  \: `Factory <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-factory-44378_>`_ \-\> `Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-election-i-36777>`)
