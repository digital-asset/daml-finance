.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-factory-holding-47403:

Module Daml.Finance.Interface.Asset.Factory.Holding
===================================================

Interfaces
----------

.. _type-daml-finance-interface-asset-factory-holding-factory-96220:

**interface** `Factory <type-daml-finance-interface-asset-factory-holding-factory-96220_>`_

  Holding factory contract used to create (credit) and archive (debit) holdings\.
  
  + **Choice Create**
    
    Create a holding on the instrument in the corresponding account\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
         - The instrument of which units are held\.
       * - account
         - :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
         - The account at which the holding is held\. Defines the holding's owner and custodian\.
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Number of units\.
       * - observers
         - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
         - Observers of the holding to be credited\.
  
  + **Choice Remove**
    
    Archive a holding\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - The parties authorizing the removal\.
       * - holdingCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`
         - The holding to be removed\.
  
  + **Method asDisclosure \:** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
    
    Conversion to ``Disclosure.I`` interface\.
  
  + **Method create' \:** Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`)
    
    Implementation of ``Create`` choice\.
  
  + **Method remove \:** Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
    
    Implementation of ``Remove`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-factory-holding-hasimplementation-73045:

**class** `Implementation <type-daml-finance-interface-asset-factory-holding-implementation-63361_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-factory-holding-hasimplementation-73045_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-asset-factory-holding-f-78374:

**type** `F <type-daml-finance-interface-asset-factory-holding-f-78374_>`_
  \= `Factory <type-daml-finance-interface-asset-factory-holding-factory-96220_>`_
  
  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-asset-factory-holding-implementation-63361:

**type** `Implementation <type-daml-finance-interface-asset-factory-holding-implementation-63361_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `F <type-daml-finance-interface-asset-factory-holding-f-78374_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Holding`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-asset-factory-holding-view-92610:

**data** `View <type-daml-finance-interface-asset-factory-holding-view-92610_>`_

  .. _constr-daml-finance-interface-asset-factory-holding-view-16945:
  
  `View <constr-daml-finance-interface-asset-factory-holding-view-16945_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-factory-holding-view-92610_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-asset-factory-holding-view-92610_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-factory-holding-view-92610_>`_

Functions
---------

.. _function-daml-finance-interface-asset-factory-holding-asdisclosure-45286:

`asDisclosure <function-daml-finance-interface-asset-factory-holding-asdisclosure-45286_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-holding-factory-96220_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-asset-factory-holding-createtick-23881:

`create' <function-daml-finance-interface-asset-factory-holding-createtick-23881_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-holding-factory-96220_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`)

.. _function-daml-finance-interface-asset-factory-holding-remove-89923:

`remove <function-daml-finance-interface-asset-factory-holding-remove-89923_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-holding-factory-96220_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
