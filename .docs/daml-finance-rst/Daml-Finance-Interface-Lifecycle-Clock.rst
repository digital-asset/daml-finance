.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-clock-75180:

Module Daml.Finance.Interface.Lifecycle.Clock
=============================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-clock-clock-52275:

**interface** `Clock <type-daml-finance-interface-lifecycle-clock-clock-52275_>`_

  A clock is a subdivision of the Time continuum into a countable set\. For each element of this set, we can calculate the corresponding UTC time\.
  
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
  

Typeclasses
-----------

.. _class-daml-finance-interface-lifecycle-clock-hasimplementation-24784:

**class** `Implementation <type-daml-finance-interface-lifecycle-clock-implementation-78570_>`_ t \=\> `HasImplementation <class-daml-finance-interface-lifecycle-clock-hasimplementation-24784_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-lifecycle-clock-i-92808:

**type** `I <type-daml-finance-interface-lifecycle-clock-i-92808_>`_
  \= `Clock <type-daml-finance-interface-lifecycle-clock-clock-52275_>`_

.. _type-daml-finance-interface-lifecycle-clock-implementation-78570:

**type** `Implementation <type-daml-finance-interface-lifecycle-clock-implementation-78570_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-lifecycle-clock-i-92808_>`_
  
  Type constraint used to require templates implementing ``Clock`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-lifecycle-clock-v-7855:

**type** `V <type-daml-finance-interface-lifecycle-clock-v-7855_>`_
  \= `View <type-daml-finance-interface-lifecycle-clock-view-77253_>`_

.. _type-daml-finance-interface-lifecycle-clock-view-77253:

**data** `View <type-daml-finance-interface-lifecycle-clock-view-77253_>`_

  View for ``Clock``\.
  
  .. _constr-daml-finance-interface-lifecycle-clock-view-66922:
  
  `View <constr-daml-finance-interface-lifecycle-clock-view-66922_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - clockTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The clock's time expressed in UTC time\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-clock-view-77253_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-lifecycle-clock-view-77253_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-clock-view-77253_>`_
