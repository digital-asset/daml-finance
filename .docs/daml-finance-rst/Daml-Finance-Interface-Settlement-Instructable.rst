.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-settlement-instructable-77681:

Module Daml.Finance.Interface.Settlement.Instructable
=====================================================

Interfaces
----------

.. _type-daml-finance-interface-settlement-instructable-instructable-17877:

**interface** `Instructable <type-daml-finance-interface-settlement-instructable-instructable-17877_>`_

  An interface used to generate settlement instructions\.
  
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
  
  + **Choice Instruct**
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - instructors
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Parties requesting to instruct a settlement\.
       * - settler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party that triggers the final settlement\.
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual identifier\.
       * - steps
         - \[:ref:`Step <type-daml-finance-interface-settlement-types-step-78661>`\]
         - Settlement steps to instruct\.
  
  + **Method instruct \: **Instruct \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-settleable-i-95514>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-instruction-i-90342>`\])
    
    Implementation of the ``Instruct`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-settlement-instructable-hasimplementation-2363:

**class** `Implementation <type-daml-finance-interface-settlement-instructable-implementation-9535_>`_ t \=\> `HasImplementation <class-daml-finance-interface-settlement-instructable-hasimplementation-2363_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-settlement-instructable-i-97939:

**type** `I <type-daml-finance-interface-settlement-instructable-i-97939_>`_
  \= `Instructable <type-daml-finance-interface-settlement-instructable-instructable-17877_>`_

.. _type-daml-finance-interface-settlement-instructable-implementation-9535:

**type** `Implementation <type-daml-finance-interface-settlement-instructable-implementation-9535_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-settlement-instructable-i-97939_>`_
  
  Type constraint used to require templates implementing ``Instructable`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-settlement-instructable-v-26212:

**type** `V <type-daml-finance-interface-settlement-instructable-v-26212_>`_
  \= `View <type-daml-finance-interface-settlement-instructable-view-99600_>`_

.. _type-daml-finance-interface-settlement-instructable-view-99600:

**data** `View <type-daml-finance-interface-settlement-instructable-view-99600_>`_

  View for ``Instructable``\.
  
  .. _constr-daml-finance-interface-settlement-instructable-view-50019:
  
  `View <constr-daml-finance-interface-settlement-instructable-view-50019_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing the facility to create settlement instructions\.
       * - observers
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Observers\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-settlement-instructable-view-99600_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-settlement-instructable-view-99600_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-settlement-instructable-view-99600_>`_

Functions
---------

.. _function-daml-finance-interface-settlement-instructable-instruct-92833:

`instruct <function-daml-finance-interface-settlement-instructable-instruct-92833_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instructable <type-daml-finance-interface-settlement-instructable-instructable-17877_>`_ \=\> t \-\> Instruct \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-settleable-i-95514>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-instruction-i-90342>`\])
