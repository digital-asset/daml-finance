.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-lifecyclable-25344:

Module Daml.Finance.Interface.Lifecycle.Lifecyclable
====================================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497:

**interface** `Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497_>`_

  Interface implemented by instruments that admit lifecycling logic\.
  
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
  
  + **Choice Lifecycle**
    
    Process an event\. It returns a tuple of the lifecycled instrument (or the original instrument when the former does not exist) and the effects\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - ruleName
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The lifecycle rule to be processed\.
       * - settler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party settling the effects\.
       * - eventCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`
         - The event\.
       * - clockCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-clock-i-92808>`
         - Current time\. This is also an observable, but not a strictly 'Decimal' one\.
       * - observableCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Observable <type-daml-finance-interface-lifecycle-observable-observable-5365>`\]
         - Set of numerical time\-dependent observables\.
  
  + **Method lifecycle \:** Lifecycle \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497_>`_, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`\])
    
    Implementation of the ``Lifecycle`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-lifecycle-lifecyclable-hasimplementation-23622:

**class** `Implementation <type-daml-finance-interface-lifecycle-lifecyclable-implementation-10942_>`_ t \=\> `HasImplementation <class-daml-finance-interface-lifecycle-lifecyclable-hasimplementation-23622_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-lifecycle-lifecyclable-i-34924:

**type** `I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924_>`_
  \= `Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497_>`_
  
  **instance** HasMethod :ref:`Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711>` \"applyElection\" (ApplyElection \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924_>`_, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`\]))
  
  **instance** HasMethod :ref:`Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711>` \"asLifecyclable\" `I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924_>`_

.. _type-daml-finance-interface-lifecycle-lifecyclable-implementation-10942:

**type** `Implementation <type-daml-finance-interface-lifecycle-lifecyclable-implementation-10942_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924_>`_
  
  Type constraint used to require templates implementing ``Lifecyclable`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-lifecycle-lifecyclable-v-15211:

**type** `V <type-daml-finance-interface-lifecycle-lifecyclable-v-15211_>`_
  \= `View <type-daml-finance-interface-lifecycle-lifecyclable-view-18993_>`_

.. _type-daml-finance-interface-lifecycle-lifecyclable-view-18993:

**data** `View <type-daml-finance-interface-lifecycle-lifecyclable-view-18993_>`_

  View for ``Lifecyclable``\.
  
  .. _constr-daml-finance-interface-lifecycle-lifecyclable-view-13908:
  
  `View <constr-daml-finance-interface-lifecycle-lifecyclable-view-13908_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - lifecycler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party performing the lifecycling\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-lifecyclable-view-18993_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-lifecycle-lifecyclable-view-18993_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-lifecyclable-view-18993_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-lifecyclable-lifecycle-43285:

`lifecycle <function-daml-finance-interface-lifecycle-lifecyclable-lifecycle-43285_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497_>`_ \=\> t \-\> Lifecycle \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497_>`_, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`\])
