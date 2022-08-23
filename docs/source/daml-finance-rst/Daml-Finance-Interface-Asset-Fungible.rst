.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-fungible-41191:

Module Daml.Finance.Interface.Asset.Fungible
============================================

Interfaces
----------

.. _type-daml-finance-interface-asset-fungible-fungible-9379:

**interface** `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_

  Interface for a fungible holding\. It requires ``Transferable``\.
  
  + **Choice ArchiveFungible**
    
    Archives the fungible contract\.
    
  
  + **Choice GetView**
    
    Returns the account's view\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - actor
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - The party fetching the view\.
  
  + **Choice Merge**
    
    Merge multiple fungible contracts into a single fungible contract\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - fungibleCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_\]
         - The fungible contracts to merge which will get consumed\.
  
  + **Choice Split**
    
    Split a fungible contract into multiple contracts by amount\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - amounts
         - \[`Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_\]
         - The quantities to split the fungible asset by, creating a new contract per amount\. The sum of the quantities is required to be smaller or equal to the amount on the fungible contract\.
  
  + **Method asTransferable \:** :ref:`I <type-daml-finance-interface-asset-transferable-i-10374>`
    
    Conversion to ``Transferable`` interface\.
  
  + **Method merge \:** Merge \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_)
    
    Implementation of the ``Merge`` choice\.
  
  + **Method split \:** Split \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_
    
    Implementation of the ``Split`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-fungible-hasimplementation-63563:

**class** `Implementation <type-daml-finance-interface-asset-fungible-implementation-98809_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-fungible-hasimplementation-63563_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-asset-fungible-hasimplementation-63563_>`_ :ref:`T <type-daml-finance-asset-fungible-t-66251>`
  
  **instance** `HasImplementation <class-daml-finance-interface-asset-fungible-hasimplementation-63563_>`_ `I <type-daml-finance-interface-asset-fungible-i-30537_>`_

Data Types
----------

.. _type-daml-finance-interface-asset-fungible-i-30537:

**type** `I <type-daml-finance-interface-asset-fungible-i-30537_>`_
  \= `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_
  
  Type synonym for ``Fungible``\.
  
  **instance** `HasImplementation <class-daml-finance-interface-asset-fungible-hasimplementation-63563_>`_ `I <type-daml-finance-interface-asset-fungible-i-30537_>`_
  
  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Fungible <type-daml-finance-asset-fungible-fungible-62518>` `I <type-daml-finance-interface-asset-fungible-i-30537_>`_
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Fungible <type-daml-finance-asset-fungible-fungible-62518>` `I <type-daml-finance-interface-asset-fungible-i-30537_>`_
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-asset-fungible-i-30537_>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-asset-fungible-i-30537_>`_ :ref:`I <type-daml-finance-interface-asset-lockable-i-23182>`
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-asset-fungible-i-30537_>`_ :ref:`I <type-daml-finance-interface-asset-transferable-i-10374>`
  
  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-asset-fungible-i-30537_>`_ :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _type-daml-finance-interface-asset-fungible-implementation-98809:

**type** `Implementation <type-daml-finance-interface-asset-fungible-implementation-98809_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-asset-fungible-i-30537_>`_, :ref:`Implementation <type-daml-finance-interface-asset-transferable-implementation-84332>` t)
  
  Type constraint used to require templates implementing ``Fungible`` to also
  implement ``Transferable``\.

.. _type-daml-finance-interface-asset-fungible-splitresult-97557:

**data** `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_

  Result of a call to ``Split``\.
  
  .. _constr-daml-finance-interface-asset-fungible-splitresult-5958:
  
  `SplitResult <constr-daml-finance-interface-asset-fungible-splitresult-5958_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - splitCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_\]
         - The contract ids for the split holdings\.
       * - rest
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_)
         - Contract id for the holding on the remaining amount\. It is ``None`` when the split is exact\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_
  
  **instance** HasMethod `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ \"split\" (Split \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_)
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t Split `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t Split `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t Split `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t Split `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_

.. _type-daml-finance-interface-asset-fungible-v-6670:

**type** `V <type-daml-finance-interface-asset-fungible-v-6670_>`_
  \= `View <type-daml-finance-interface-asset-fungible-view-83962_>`_
  
  Type synonym for ``View``\.

.. _type-daml-finance-interface-asset-fungible-view-83962:

**data** `View <type-daml-finance-interface-asset-fungible-view-83962_>`_

  View for ``Fungible``\.
  
  .. _constr-daml-finance-interface-asset-fungible-view-90655:
  
  `View <constr-daml-finance-interface-asset-fungible-view-90655_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - modifiers
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Parties which have the authorization to modify a fungible asset\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-fungible-view-83962_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-asset-fungible-view-83962_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-fungible-view-83962_>`_
  
  **instance** `HasInterfaceView <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfaceview-4492>`_ `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ `View <type-daml-finance-interface-asset-fungible-view-83962_>`_
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t GetView `View <type-daml-finance-interface-asset-fungible-view-83962_>`_
  
  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t GetView `View <type-daml-finance-interface-asset-fungible-view-83962_>`_
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t GetView `View <type-daml-finance-interface-asset-fungible-view-83962_>`_
  
  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t GetView `View <type-daml-finance-interface-asset-fungible-view-83962_>`_

Functions
---------

.. _function-daml-finance-interface-asset-fungible-astransferable-40294:

`asTransferable <function-daml-finance-interface-asset-fungible-astransferable-40294_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-asset-transferable-i-10374>`

.. _function-daml-finance-interface-asset-fungible-split-17428:

`split <function-daml-finance-interface-asset-fungible-split-17428_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ \=\> t \-\> Split \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `SplitResult <type-daml-finance-interface-asset-fungible-splitresult-97557_>`_

.. _function-daml-finance-interface-asset-fungible-merge-2236:

`merge <function-daml-finance-interface-asset-fungible-merge-2236_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_ \=\> t \-\> Merge \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_)

.. _function-daml-finance-interface-asset-fungible-verifysplit-93847:

`verifySplit <function-daml-finance-interface-asset-fungible-verifysplit-93847_>`_
  \: `CanAssert <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-assert-canassert-67323>`_ m \=\> \[`Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_\] \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> m ()
  
  Verifies that split amounts are strictly positive and don't exceed the current amount\.

.. _function-daml-finance-interface-asset-fungible-verifyamountpreserving-12785:

`verifyAmountPreserving <function-daml-finance-interface-asset-fungible-verifyamountpreserving-12785_>`_
  \: `CanAssert <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-assert-canassert-67323>`_ m \=\> \[`Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_\] \-\> \[`Fungible <type-daml-finance-interface-asset-fungible-fungible-9379_>`_\] \-\> m ()
  
  Verifies that the original amount is preserved after a merge or a split\.
