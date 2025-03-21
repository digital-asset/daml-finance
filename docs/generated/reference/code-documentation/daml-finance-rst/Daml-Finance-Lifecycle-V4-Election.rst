.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-election-2732:

Daml.Finance.Lifecycle.V4.Election
==================================

Templates
---------

.. _type-daml-finance-lifecycle-v4-election-election-87911:

**template** `Election <type-daml-finance-lifecycle-v4-election-election-87911_>`_

  An election, such as the exercise of an option\.

  Signatory\: elector, provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - elector
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Entity making the election\.
     * - counterparty
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Faces the ``elector`` in the ``Holding``\.
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The provider of the election is an entity that has the authority to process the election and create a new instrument version\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Election identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A human readable description of the election\.
     * - instrument
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The instrument to which the election applies\.
     * - amount
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - Number of units of instrument to which the election applies\.
     * - claim
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The tag corresponding to the elected sub\-tree\.
     * - electorIsOwner
       - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
       - ``True`` if the elector is the owner of a claim, ``False`` otherwise\.
     * - electionTime
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Time at which the election is put forward\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - A set of observers\.

  + **Choice** Archive

    Controller\: elector, provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-election-i-36777>` **for** `Election <type-daml-finance-lifecycle-v4-election-election-87911_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>` **for** `Election <type-daml-finance-lifecycle-v4-election-election-87911_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Election <type-daml-finance-lifecycle-v4-election-election-87911_>`_

.. _type-daml-finance-lifecycle-v4-election-factory-68585:

**template** `Factory <type-daml-finance-lifecycle-v4-election-factory-68585_>`_

  Factory template to create an ``Election``\.

  Signatory\: provider

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
       - A set of observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-election-factory-i-34597>` **for** `Factory <type-daml-finance-lifecycle-v4-election-factory-68585_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-lifecycle-v4-election-factory-68585_>`_

Data Types
----------

.. _type-daml-finance-lifecycle-v4-election-t-50041:

**type** `T <type-daml-finance-lifecycle-v4-election-t-50041_>`_
  \= `Election <type-daml-finance-lifecycle-v4-election-election-87911_>`_

  Type synonym for ``Election``\.
