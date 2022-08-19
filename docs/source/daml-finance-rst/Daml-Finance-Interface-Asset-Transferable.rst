.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-transferable-44858:

Module Daml.Finance.Interface.Asset.Transferable
================================================

Interfaces
----------

.. _type-daml-finance-interface-asset-transferable-transferable-34689:

**interface** `Transferable <type-daml-finance-interface-asset-transferable-transferable-34689_>`_

  An interface respresenting a contract where ownership can be transfered to other parties
  
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
  
  + **Choice Transfer**
    
    Transfer a contract to a new owner
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - newOwnerAccount
         - :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
         - Account contract id of the parties to transfer the contract to
  
  + **Method asLockable \:** :ref:`I <type-daml-finance-interface-asset-lockable-i-23182>`
    
    Conversion to ``Lockable`` interface\.
  
  + **Method transfer \:** Transfer \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Transferable <type-daml-finance-interface-asset-transferable-transferable-34689_>`_)
    
    Implementation of the ``Transfer`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-transferable-hasimplementation-59736:

**class** `Implementation <type-daml-finance-interface-asset-transferable-implementation-84332_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-transferable-hasimplementation-59736_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-asset-transferable-i-10374:

**type** `I <type-daml-finance-interface-asset-transferable-i-10374_>`_
  \= `Transferable <type-daml-finance-interface-asset-transferable-transferable-34689_>`_
  
  **instance** HasMethod :ref:`Fungible <type-daml-finance-interface-asset-fungible-fungible-9379>` \"asTransferable\" `I <type-daml-finance-interface-asset-transferable-i-10374_>`_
  
  **instance** HasMethod :ref:`Instruction <type-daml-finance-interface-settlement-instruction-instruction-30569>` \"execute\" (`Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-transferable-i-10374_>`_))
  
  **instance** HasMethod :ref:`Settleable <type-daml-finance-interface-settlement-settleable-settleable-40815>` \"settle\" (`Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-transferable-i-10374_>`_\])

.. _type-daml-finance-interface-asset-transferable-implementation-84332:

**type** `Implementation <type-daml-finance-interface-asset-transferable-implementation-84332_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-asset-transferable-i-10374_>`_, :ref:`Implementation <type-daml-finance-interface-asset-lockable-implementation-3140>` t)
  
  Type constraint used to require templates implementing ``Transferable`` to
  also implement ``Lockable``\.

.. _type-daml-finance-interface-asset-transferable-v-3761:

**type** `V <type-daml-finance-interface-asset-transferable-v-3761_>`_
  \= `View <type-daml-finance-interface-asset-transferable-view-98695_>`_

.. _type-daml-finance-interface-asset-transferable-view-98695:

**data** `View <type-daml-finance-interface-asset-transferable-view-98695_>`_

  View for ``Transferable``\.
  
  .. _constr-daml-finance-interface-asset-transferable-view-38614:
  
  `View <constr-daml-finance-interface-asset-transferable-view-38614_>`_ ()
  
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-transferable-view-98695_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-asset-transferable-view-98695_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-transferable-view-98695_>`_

Functions
---------

.. _function-daml-finance-interface-asset-transferable-aslockable-68487:

`asLockable <function-daml-finance-interface-asset-transferable-aslockable-68487_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Transferable <type-daml-finance-interface-asset-transferable-transferable-34689_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-asset-lockable-i-23182>`

.. _function-daml-finance-interface-asset-transferable-transfer-4365:

`transfer <function-daml-finance-interface-asset-transferable-transfer-4365_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Transferable <type-daml-finance-interface-asset-transferable-transferable-34689_>`_ \=\> t \-\> Transfer \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Transferable <type-daml-finance-interface-asset-transferable-transferable-34689_>`_)
