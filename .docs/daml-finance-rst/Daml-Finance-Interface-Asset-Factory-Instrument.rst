.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-factory-instrument-15762:

Module Daml.Finance.Interface.Asset.Factory.Instrument
======================================================

Interfaces
----------

.. _type-daml-finance-interface-asset-factory-instrument-factory-88339:

**interface** `Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339_>`_

  Interface that allows implementing templates to create instruments\.
  
  + **Choice Create**
    
    Create a new account\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
         - The instrument's key\.
       * - validAsOf
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Timestamp as of which the instrument is valid\.
       * - observers
         - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
         - The instrument's observers\.
  
  + **Choice Remove**
    
    Archive an account\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
         - The account's key\.
  
  + **Method asDisclosure \: **:ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
    
    Conversion to ``Disclosure`` interface\.
  
  + **Method create' \: **Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`)
    
    Implementation of ``Create`` choice\.
  
  + **Method remove \: **Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
    
    Implementation of ``Remove`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-factory-instrument-hasimplementation-41720:

**class** `Implementation <type-daml-finance-interface-asset-factory-instrument-implementation-86332_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-factory-instrument-hasimplementation-41720_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-asset-factory-instrument-f-87857:

**type** `F <type-daml-finance-interface-asset-factory-instrument-f-87857_>`_
  \= `Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339_>`_
  
  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-asset-factory-instrument-implementation-86332:

**type** `Implementation <type-daml-finance-interface-asset-factory-instrument-implementation-86332_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Factory`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-asset-factory-instrument-view-16567:

**data** `View <type-daml-finance-interface-asset-factory-instrument-view-16567_>`_

  .. _constr-daml-finance-interface-asset-factory-instrument-view-21518:
  
  `View <constr-daml-finance-interface-asset-factory-instrument-view-21518_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-factory-instrument-view-16567_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-asset-factory-instrument-view-16567_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-factory-instrument-view-16567_>`_

Functions
---------

.. _function-daml-finance-interface-asset-factory-instrument-asdisclosure-47075:

`asDisclosure <function-daml-finance-interface-asset-factory-instrument-asdisclosure-47075_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-asset-factory-instrument-createtick-98438:

`create' <function-daml-finance-interface-asset-factory-instrument-createtick-98438_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`)

.. _function-daml-finance-interface-asset-factory-instrument-remove-35078:

`remove <function-daml-finance-interface-asset-factory-instrument-remove-35078_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
