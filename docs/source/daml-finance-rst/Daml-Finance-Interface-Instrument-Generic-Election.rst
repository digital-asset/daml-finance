.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-election-77226:

Module Daml.Finance.Interface.Instrument.Generic.Election
=================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-generic-election-election-60525:

**interface** `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_

  Interface implemented by templates that represents a (claim\-based) election\.

  + **Choice Apply**

    applies the election to the instrument, returning the new instrument as well
    as the corresponding effects\. The election is archived as part of this choice\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - clockCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-clock-i-92808>`
         - current time\.
       * - observableCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-observable-i-63746>`\]
         - set of observables
       * - settler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - parties responsible for settling effects

  + **Choice GetView**

    Retrieves the instrument view\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method archive' \:**\ `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

    Implementation of archiving the contract\.

  + **Method asEvent \:**\ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

    Conversion to ``Event`` interface\.

.. _type-daml-finance-interface-instrument-generic-election-exercisable-60012:

**interface** `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_

  Interface implemented by instruments that admit (claim\-based) elections\.

  + **Choice ApplyElection**

    Applies an election to the instrument\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - clockCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-clock-i-92808>`
         - Current time\. This is also an observable, but not a strictly 'Decimal' one\.
       * - electionCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_
         - The election\.
       * - observableCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-observable-i-63746>`\]
         - Set of observables\.
       * - settler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party settling the transaction\.

  + **Choice Exercisable\_GetView**

    Retrieves the instrument view\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method applyElection \:**\ ApplyElection \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`\])

    Implementation of the ``ApplyElection`` choice\.

  + **Method asLifecyclable \:**\ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

    Conversion to ``Lifecyclable`` interface\.

Typeclasses
-----------

.. _class-daml-finance-interface-instrument-generic-election-hasimplementation-75558:

**class** `Implementation <type-daml-finance-interface-instrument-generic-election-implementation-12148_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-generic-election-hasimplementation-75558_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-instrument-generic-election-hasimplementation-75558_>`_ :ref:`T <type-daml-finance-instrument-generic-election-t-54254>`

  **instance** `HasImplementation <class-daml-finance-interface-instrument-generic-election-hasimplementation-75558_>`_ `I <type-daml-finance-interface-instrument-generic-election-i-77182_>`_

.. _class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-7032:

**class** `ExercisableImplementation <type-daml-finance-interface-instrument-generic-election-exercisableimplementation-69980_>`_ t \=\> `ExercisableHasImplementation <class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-7032_>`_ t **where**

  **instance** `ExercisableHasImplementation <class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-7032_>`_ :ref:`T <type-daml-finance-instrument-generic-instrument-t-62954>`

  **instance** `ExercisableHasImplementation <class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-7032_>`_ `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_

Data Types
----------

.. _type-daml-finance-interface-instrument-generic-election-exercisableimplementation-69980:

**type** `ExercisableImplementation <type-daml-finance-interface-instrument-generic-election-exercisableimplementation-69980_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_, :ref:`Implementation <type-daml-finance-interface-lifecycle-lifecyclable-implementation-10942>` t)

  Type constraint used to require templates implementing ``Exercisable`` to also
  implement ``Lifecyclable``\.

.. _type-daml-finance-interface-instrument-generic-election-exercisableview-24463:

**data** `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

  View for ``Exercisable``\.

  .. _constr-daml-finance-interface-instrument-generic-election-exercisableview-41490:

  `ExercisableView <constr-daml-finance-interface-instrument-generic-election-exercisableview-41490_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lifecycler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party processing the election\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

  **instance** HasInterfaceView `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_ `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t Exercisable\_GetView `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t Exercisable\_GetView `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t Exercisable\_GetView `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t Exercisable\_GetView `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-24463_>`_

.. _type-daml-finance-interface-instrument-generic-election-i-77182:

**type** `I <type-daml-finance-interface-instrument-generic-election-i-77182_>`_
  \= `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_

  **instance** `HasImplementation <class-daml-finance-interface-instrument-generic-election-hasimplementation-75558_>`_ `I <type-daml-finance-interface-instrument-generic-election-i-77182_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Election <type-daml-finance-instrument-generic-election-election-47518>` `I <type-daml-finance-interface-instrument-generic-election-i-77182_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Election <type-daml-finance-instrument-generic-election-election-47518>` `I <type-daml-finance-interface-instrument-generic-election-i-77182_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-instrument-generic-election-i-77182_>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

.. _type-daml-finance-interface-instrument-generic-election-implementation-12148:

**type** `Implementation <type-daml-finance-interface-instrument-generic-election-implementation-12148_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-instrument-generic-election-i-77182_>`_, :ref:`Implementation <type-daml-finance-interface-lifecycle-event-implementation-22192>` t)

  Type constraint used to require templates implementing ``Exercisable`` to also
  implement ``Event``\.

.. _type-daml-finance-interface-instrument-generic-election-v-40089:

**type** `V <type-daml-finance-interface-instrument-generic-election-v-40089_>`_
  \= `View <type-daml-finance-interface-instrument-generic-election-view-34831_>`_

.. _type-daml-finance-interface-instrument-generic-election-view-34831:

**data** `View <type-daml-finance-interface-instrument-generic-election-view-34831_>`_

  View for ``Election``\.

  .. _constr-daml-finance-interface-instrument-generic-election-view-4056:

  `View <constr-daml-finance-interface-instrument-generic-election-view-4056_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - claim
         - :ref:`C <type-daml-finance-interface-instrument-generic-types-c-63687>`
         - The claim representation of the election (i\.e\., the elected sub\-tree)\.
       * - elector
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - parties on behalf of which the election is made
       * - counterparty
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - faces the ``elector`` in the ``Holding``
       * - electorIsOwner
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - ``True`` if election is on behalf of the owner of the holding, ``False`` otherwise
       * - observers
         - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
         - observers of the election
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - number of instrument units to which the election applies
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - party that is authorized to process the election and generate the new instrument version \+ effects
       * - instrument
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-75164>`
         - the instrument to which the election applies

  **instance** HasInterfaceView `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_ `View <type-daml-finance-interface-instrument-generic-election-view-34831_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t GetView `View <type-daml-finance-interface-instrument-generic-election-view-34831_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t GetView `View <type-daml-finance-interface-instrument-generic-election-view-34831_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t GetView `View <type-daml-finance-interface-instrument-generic-election-view-34831_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t GetView `View <type-daml-finance-interface-instrument-generic-election-view-34831_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-generic-election-asevent-78817:

`asEvent <function-daml-finance-interface-instrument-generic-election-asevent-78817_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

.. _function-daml-finance-interface-instrument-generic-election-archivetick-49277:

`archive' <function-daml-finance-interface-instrument-generic-election-archivetick-49277_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_ \=\> t \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

.. _function-daml-finance-interface-instrument-generic-election-getelectiontime-24715:

`getElectionTime <function-daml-finance-interface-instrument-generic-election-getelectiontime-24715_>`_
  \: `Election <type-daml-finance-interface-instrument-generic-election-election-60525_>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  Retrieves the election's time\.

.. _function-daml-finance-interface-instrument-generic-election-aslifecyclable-39169:

`asLifecyclable <function-daml-finance-interface-instrument-generic-election-aslifecyclable-39169_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

.. _function-daml-finance-interface-instrument-generic-election-applyelection-39382:

`applyElection <function-daml-finance-interface-instrument-generic-election-applyelection-39382_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_ \=\> t \-\> ApplyElection \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-60012_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`\])
