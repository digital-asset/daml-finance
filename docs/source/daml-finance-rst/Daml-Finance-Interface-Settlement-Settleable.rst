.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-settlement-settleable-55322:

Module Daml.Finance.Interface.Settlement.Settleable
===================================================

Interfaces
----------

.. _type-daml-finance-interface-settlement-settleable-settleable-40815:

**interface** `Settleable <type-daml-finance-interface-settlement-settleable-settleable-40815_>`_

  An interface for atomically settling ``Transferable``\\s\.
  
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
  
  + **Choice Settle**
    
    Execute settlement\.
    
  
  + **Method settle \:** `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-transferable-i-10374>`\]
    
    Implementation of the ``Settle`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-settlement-settleable-hasimplementation-37118:

**class** `Implementation <type-daml-finance-interface-settlement-settleable-implementation-98032_>`_ t \=\> `HasImplementation <class-daml-finance-interface-settlement-settleable-hasimplementation-37118_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-settlement-settleable-i-95514:

**type** `I <type-daml-finance-interface-settlement-settleable-i-95514_>`_
  \= `Settleable <type-daml-finance-interface-settlement-settleable-settleable-40815_>`_
  
  **instance** HasMethod :ref:`Instructable <type-daml-finance-interface-settlement-instructable-instructable-17877>` \"instruct\" (Instruct \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-settlement-settleable-i-95514_>`_, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-instruction-i-90342>`\]))

.. _type-daml-finance-interface-settlement-settleable-implementation-98032:

**type** `Implementation <type-daml-finance-interface-settlement-settleable-implementation-98032_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-settlement-settleable-i-95514_>`_
  
  Type constraint used to require templates implementing ``Settleable`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-settlement-settleable-v-93181:

**type** `V <type-daml-finance-interface-settlement-settleable-v-93181_>`_
  \= `View <type-daml-finance-interface-settlement-settleable-view-20035_>`_

.. _type-daml-finance-interface-settlement-settleable-view-20035:

**data** `View <type-daml-finance-interface-settlement-settleable-view-20035_>`_

  View for ``Settleable``\.
  
  .. _constr-daml-finance-interface-settlement-settleable-view-5308:
  
  `View <constr-daml-finance-interface-settlement-settleable-view-5308_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - settler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party triggering the settlement\.
       * - steps
         - \[:ref:`Step <type-daml-finance-interface-settlement-types-step-78661>`\]
         - Settlement steps\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-settlement-settleable-view-20035_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-settlement-settleable-view-20035_>`_

Functions
---------

.. _function-daml-finance-interface-settlement-settleable-settle-16835:

`settle <function-daml-finance-interface-settlement-settleable-settle-16835_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Settleable <type-daml-finance-interface-settlement-settleable-settleable-40815_>`_ \=\> t \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-transferable-i-10374>`\]
