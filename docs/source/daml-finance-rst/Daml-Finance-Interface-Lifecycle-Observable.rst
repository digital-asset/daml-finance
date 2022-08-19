.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-observable-3374:

Module Daml.Finance.Interface.Lifecycle.Observable
==================================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-observable-observable-5365:

**interface** `Observable <type-daml-finance-interface-lifecycle-observable-observable-5365_>`_

  An inferface to inspect some numerical values required as part of processing a lifecycle rule\.
  
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
  
  + **Choice Observe**
    
    Observe the ``Observable``\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Parties calling this 'Observe' choice\.
       * - t
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Time at which the value is observed\.
  
  + **Method observe \:** `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
    
    Implementation of the ``Observe`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-lifecycle-observable-hasimplementation-5664:

**class** `Implementation <type-daml-finance-interface-lifecycle-observable-implementation-63784_>`_ t \=\> `HasImplementation <class-daml-finance-interface-lifecycle-observable-hasimplementation-5664_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-lifecycle-observable-hasimplementation-5664_>`_ `I <type-daml-finance-interface-lifecycle-observable-i-63746_>`_
  
  **instance** `HasImplementation <class-daml-finance-interface-lifecycle-observable-hasimplementation-5664_>`_ T

Data Types
----------

.. _type-daml-finance-interface-lifecycle-observable-i-63746:

**type** `I <type-daml-finance-interface-lifecycle-observable-i-63746_>`_
  \= `Observable <type-daml-finance-interface-lifecycle-observable-observable-5365_>`_
  
  **instance** `HasImplementation <class-daml-finance-interface-lifecycle-observable-hasimplementation-5664_>`_ `I <type-daml-finance-interface-lifecycle-observable-i-63746_>`_
  
  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Observation <type-daml-finance-refdata-observation-observation-39199>` `I <type-daml-finance-interface-lifecycle-observable-i-63746_>`_
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Observation <type-daml-finance-refdata-observation-observation-39199>` `I <type-daml-finance-interface-lifecycle-observable-i-63746_>`_

.. _type-daml-finance-interface-lifecycle-observable-implementation-63784:

**type** `Implementation <type-daml-finance-interface-lifecycle-observable-implementation-63784_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-lifecycle-observable-i-63746_>`_
  
  Type constraint used to require templates implementing ``Observable`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-lifecycle-observable-v-91893:

**type** `V <type-daml-finance-interface-lifecycle-observable-v-91893_>`_
  \= `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_

.. _type-daml-finance-interface-lifecycle-observable-view-60923:

**data** `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_

  View for ``Observable``\.
  
  .. _constr-daml-finance-interface-lifecycle-observable-view-56926:
  
  `View <constr-daml-finance-interface-lifecycle-observable-view-56926_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing the observables\.
       * - obsKey
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Textual reference to the observable\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_
  
  **instance** `HasInterfaceView <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfaceview-4492>`_ `Observable <type-daml-finance-interface-lifecycle-observable-observable-5365_>`_ `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Observable <type-daml-finance-interface-lifecycle-observable-observable-5365_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t GetView `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Observable <type-daml-finance-interface-lifecycle-observable-observable-5365_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t GetView `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Observable <type-daml-finance-interface-lifecycle-observable-observable-5365_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t GetView `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Observable <type-daml-finance-interface-lifecycle-observable-observable-5365_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t GetView `View <type-daml-finance-interface-lifecycle-observable-view-60923_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-observable-observe-6465:

`observe <function-daml-finance-interface-lifecycle-observable-observe-6465_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Observable <type-daml-finance-interface-lifecycle-observable-observable-5365_>`_ \=\> t \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
