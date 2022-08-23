.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-election-94835:

Module Daml.Finance.Interface.Instrument.Generic.Election
=========================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-generic-election-election-25324:

**interface** `Election <type-daml-finance-interface-instrument-generic-election-election-25324_>`_

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
  
  + **Method archive' \:** `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-instrument-generic-election-election-25324_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
    
    Implementation of archiving the contract\.
  
  + **Method asEvent \:** :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`
    
    Conversion to ``Event`` interface\.

.. _type-daml-finance-interface-instrument-generic-election-exercisable-33711:

**interface** `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711_>`_

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
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-instrument-generic-election-election-25324_>`_
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
  
  + **Method applyElection \:** ApplyElection \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`\])
    
    Implementation of the ``ApplyElection`` choice\.
  
  + **Method asLifecyclable \:** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`
    
    Conversion to ``Lifecyclable`` interface\.

Typeclasses
-----------

.. _class-daml-finance-interface-instrument-generic-election-hasimplementation-97189:

**class** `Implementation <type-daml-finance-interface-instrument-generic-election-implementation-58245_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-generic-election-hasimplementation-97189_>`_ t **where**


.. _class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-16317:

**class** `ExercisableImplementation <type-daml-finance-interface-instrument-generic-election-exercisableimplementation-45207_>`_ t \=\> `ExercisableHasImplementation <class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-16317_>`_ t **where**

  **instance** `ExercisableHasImplementation <class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-16317_>`_ :ref:`T <type-daml-finance-instrument-generic-instrument-t-12893>`
  
  **instance** `ExercisableHasImplementation <class-daml-finance-interface-instrument-generic-election-exercisablehasimplementation-16317_>`_ `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711_>`_

Data Types
----------

.. _type-daml-finance-interface-instrument-generic-election-exercisableimplementation-45207:

**type** `ExercisableImplementation <type-daml-finance-interface-instrument-generic-election-exercisableimplementation-45207_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711_>`_, :ref:`Implementation <type-daml-finance-interface-lifecycle-lifecyclable-implementation-10942>` t)
  
  Type constraint used to require templates implementing ``Exercisable`` to also
  implement ``Lifecyclable``\.

.. _type-daml-finance-interface-instrument-generic-election-exercisableview-40272:

**data** `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-40272_>`_

  View for ``Exercisable``\.
  
  .. _constr-daml-finance-interface-instrument-generic-election-exercisableview-59909:
  
  `ExercisableView <constr-daml-finance-interface-instrument-generic-election-exercisableview-59909_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - lifecycler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party processing the election\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-40272_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-40272_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ExercisableView <type-daml-finance-interface-instrument-generic-election-exercisableview-40272_>`_

.. _type-daml-finance-interface-instrument-generic-election-i-85653:

**type** `I <type-daml-finance-interface-instrument-generic-election-i-85653_>`_
  \= `Election <type-daml-finance-interface-instrument-generic-election-election-25324_>`_

.. _type-daml-finance-interface-instrument-generic-election-implementation-58245:

**type** `Implementation <type-daml-finance-interface-instrument-generic-election-implementation-58245_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-instrument-generic-election-i-85653_>`_, :ref:`Implementation <type-daml-finance-interface-lifecycle-event-implementation-22192>` t)
  
  Type constraint used to require templates implementing ``Exercisable`` to also
  implement ``Event``\.

.. _type-daml-finance-interface-instrument-generic-election-v-57506:

**type** `V <type-daml-finance-interface-instrument-generic-election-v-57506_>`_
  \= `View <type-daml-finance-interface-instrument-generic-election-view-99038_>`_

.. _type-daml-finance-interface-instrument-generic-election-view-99038:

**data** `View <type-daml-finance-interface-instrument-generic-election-view-99038_>`_

  View for ``Election``\.
  
  .. _constr-daml-finance-interface-instrument-generic-election-view-25157:
  
  `View <constr-daml-finance-interface-instrument-generic-election-view-25157_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - claim
         - :ref:`C <type-daml-finance-interface-instrument-generic-types-c-8090>`
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
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-58546>`
         - the instrument to which the election applies

Functions
---------

.. _function-daml-finance-interface-instrument-generic-election-asevent-91514:

`asEvent <function-daml-finance-interface-instrument-generic-election-asevent-91514_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Election <type-daml-finance-interface-instrument-generic-election-election-25324_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

.. _function-daml-finance-interface-instrument-generic-election-archivetick-16176:

`archive' <function-daml-finance-interface-instrument-generic-election-archivetick-16176_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Election <type-daml-finance-interface-instrument-generic-election-election-25324_>`_ \=\> t \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Election <type-daml-finance-interface-instrument-generic-election-election-25324_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

.. _function-daml-finance-interface-instrument-generic-election-getelectiontime-13640:

`getElectionTime <function-daml-finance-interface-instrument-generic-election-getelectiontime-13640_>`_
  \: `Election <type-daml-finance-interface-instrument-generic-election-election-25324_>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
  
  Retrieves the election's time\.

.. _function-daml-finance-interface-instrument-generic-election-aslifecyclable-4576:

`asLifecyclable <function-daml-finance-interface-instrument-generic-election-aslifecyclable-4576_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

.. _function-daml-finance-interface-instrument-generic-election-applyelection-46753:

`applyElection <function-daml-finance-interface-instrument-generic-election-applyelection-46753_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711_>`_ \=\> t \-\> ApplyElection \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`\])
