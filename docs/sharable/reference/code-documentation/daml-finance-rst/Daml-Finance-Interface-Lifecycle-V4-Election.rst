.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-election-15483:

Daml.Finance.Interface.Lifecycle.V4.Election
============================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-election-election-99800:

**interface** `Election <type-daml-finance-interface-lifecycle-v4-election-election-99800_>`_

  Interface implemented by templates that represents a claim\-based election\. This interface
  requires the ``Event`` interface implementation\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-election-v-12910_>`_

  + .. _type-daml-finance-interface-lifecycle-v4-election-apply-6828:

    **Choice** `Apply <type-daml-finance-interface-lifecycle-v4-election-apply-6828_>`_

    Applies the election to the instrument, returning the new instrument as well as the
    corresponding effects\. The election is archived as part of this choice\.

    Controller\: (DA\.Internal\.Record\.getField @\"provider\" (view this))

    Returns\: (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - observableCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-i-61855>`\]
         - Set of observables\.
       * - exercisableCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Exercisable <type-daml-finance-interface-lifecycle-v4-election-exercisable-36259_>`_
         - The contract that is used to apply an election to the instrument\.

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-election-getview-69321:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-election-getview-69321_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-election-view-84858_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method apply \:** `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-lifecycle-v4-election-election-99800_>`_ \-\> `Apply <type-daml-finance-interface-lifecycle-v4-election-apply-6828_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])

    Implementation of the ``Apply`` choice\.

.. _type-daml-finance-interface-lifecycle-v4-election-exercisable-36259:

**interface** `Exercisable <type-daml-finance-interface-lifecycle-v4-election-exercisable-36259_>`_

  Interface implemented by instruments that admit (claim\-based) elections\.

  **viewtype** `ExercisableView <type-daml-finance-interface-lifecycle-v4-election-exercisableview-99924_>`_

  + .. _type-daml-finance-interface-lifecycle-v4-election-applyelection-69809:

    **Choice** `ApplyElection <type-daml-finance-interface-lifecycle-v4-election-applyelection-69809_>`_

    Applies an election to the instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"lifecycler\" (view this))

    Returns\: (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - electionCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-lifecycle-v4-election-election-99800_>`_
         - The election\.
       * - observableCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-i-61855>`\]
         - Set of observables\.

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-election-exercisablegetview-56501:

    **Choice** `Exercisable_GetView <type-daml-finance-interface-lifecycle-v4-election-exercisablegetview-56501_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `ExercisableView <type-daml-finance-interface-lifecycle-v4-election-exercisableview-99924_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method applyElection \:** `ApplyElection <type-daml-finance-interface-lifecycle-v4-election-applyelection-69809_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])

    Implementation of the ``ApplyElection`` choice\.

Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-election-exercisableview-99924:

**data** `ExercisableView <type-daml-finance-interface-lifecycle-v4-election-exercisableview-99924_>`_

  View for ``Exercisable``\.

  .. _constr-daml-finance-interface-lifecycle-v4-election-exercisableview-60985:

  `ExercisableView <constr-daml-finance-interface-lifecycle-v4-election-exercisableview-60985_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lifecycler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party processing the election\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ExercisableView <type-daml-finance-interface-lifecycle-v4-election-exercisableview-99924_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ExercisableView <type-daml-finance-interface-lifecycle-v4-election-exercisableview-99924_>`_

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Exercisable <type-daml-finance-interface-lifecycle-v4-election-exercisable-36259_>`_ `ExercisableView <type-daml-finance-interface-lifecycle-v4-election-exercisableview-99924_>`_

.. _type-daml-finance-interface-lifecycle-v4-election-i-36777:

**type** `I <type-daml-finance-interface-lifecycle-v4-election-i-36777_>`_
  \= `Election <type-daml-finance-interface-lifecycle-v4-election-election-99800_>`_

  Type synonym for ``Election``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-factory-44378>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-election-factory-create-69397>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-v4-election-i-36777_>`_))

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-lifecycle-v4-election-factory-factory-95552>` \"create'\" (:ref:`Create <type-daml-finance-interface-lifecycle-v4-election-factory-create-20391>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-v4-election-i-36777_>`_))

.. _type-daml-finance-interface-lifecycle-v4-election-v-12910:

**type** `V <type-daml-finance-interface-lifecycle-v4-election-v-12910_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-election-view-84858_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Election <type-daml-finance-interface-lifecycle-v4-election-election-99800_>`_ `V <type-daml-finance-interface-lifecycle-v4-election-v-12910_>`_

.. _type-daml-finance-interface-lifecycle-v4-election-view-84858:

**data** `View <type-daml-finance-interface-lifecycle-v4-election-view-84858_>`_

  View for ``Election``\.

  .. _constr-daml-finance-interface-lifecycle-v4-election-view-24289:

  `View <constr-daml-finance-interface-lifecycle-v4-election-view-24289_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - The identifier for an election\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A description of the instrument\.
       * - claim
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The tag corresponding to the elected sub\-tree\.
       * - elector
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Parties on behalf of which the election is made\.
       * - counterparty
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Faces the ``elector`` in the ``Holding``\.
       * - electorIsOwner
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - ``True`` if election is on behalf of the owner of the holding, ``False`` otherwise\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - Observers of the election\.
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Number of instrument units to which the election applies\.
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party that is authorized to process the election and generate the new instrument version and effects\.
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument to which the election applies\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-election-view-84858_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-election-view-84858_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-v4-election-apply-60232:

`apply <function-daml-finance-interface-lifecycle-v4-election-apply-60232_>`_
  \: `Election <type-daml-finance-interface-lifecycle-v4-election-election-99800_>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-lifecycle-v4-election-election-99800_>`_ \-\> `Apply <type-daml-finance-interface-lifecycle-v4-election-apply-6828_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])

.. _function-daml-finance-interface-lifecycle-v4-election-getelectiontime-70060:

`getElectionTime <function-daml-finance-interface-lifecycle-v4-election-getelectiontime-70060_>`_
  \: `Election <type-daml-finance-interface-lifecycle-v4-election-election-99800_>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  Retrieves the election's time\.

.. _function-daml-finance-interface-lifecycle-v4-election-applyelection-80909:

`applyElection <function-daml-finance-interface-lifecycle-v4-election-applyelection-80909_>`_
  \: `Exercisable <type-daml-finance-interface-lifecycle-v4-election-exercisable-36259_>`_ \-\> `ApplyElection <type-daml-finance-interface-lifecycle-v4-election-applyelection-69809_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])
