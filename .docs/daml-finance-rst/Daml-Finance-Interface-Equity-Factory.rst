.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-equity-factory-51096:

Module Daml.Finance.Interface.Equity.Factory
============================================

Interfaces
----------

.. _type-daml-finance-interface-equity-factory-factory-50265:

**interface** `Factory <type-daml-finance-interface-equity-factory-factory-50265_>`_

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
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
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

.. _class-daml-finance-interface-equity-factory-hasimplementation-69542:

**class** `Implementation <type-daml-finance-interface-equity-factory-implementation-29362_>`_ t \=\> `HasImplementation <class-daml-finance-interface-equity-factory-hasimplementation-69542_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-equity-factory-f-7879:

**type** `F <type-daml-finance-interface-equity-factory-f-7879_>`_
  \= `Factory <type-daml-finance-interface-equity-factory-factory-50265_>`_
  
  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-equity-factory-implementation-29362:

**type** `Implementation <type-daml-finance-interface-equity-factory-implementation-29362_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Factory <type-daml-finance-interface-equity-factory-factory-50265_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Factory`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-equity-factory-view-89005:

**data** `View <type-daml-finance-interface-equity-factory-view-89005_>`_

  .. _constr-daml-finance-interface-equity-factory-view-54088:
  
  `View <constr-daml-finance-interface-equity-factory-view-54088_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-equity-factory-view-89005_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-equity-factory-view-89005_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-equity-factory-view-89005_>`_

Functions
---------

.. _function-daml-finance-interface-equity-factory-asdisclosure-34905:

`asDisclosure <function-daml-finance-interface-equity-factory-asdisclosure-34905_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-equity-factory-factory-50265_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-equity-factory-createtick-90952:

`create' <function-daml-finance-interface-equity-factory-createtick-90952_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-equity-factory-factory-50265_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`)

.. _function-daml-finance-interface-equity-factory-remove-44012:

`remove <function-daml-finance-interface-equity-factory-remove-44012_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-equity-factory-factory-50265_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
