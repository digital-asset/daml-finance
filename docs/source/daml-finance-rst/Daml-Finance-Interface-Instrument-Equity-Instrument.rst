.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-equity-instrument-13224:

Module Daml.Finance.Interface.Instrument.Equity.Instrument
==========================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-equity-instrument-instrument-99859:

**interface** `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-99859_>`_

  An interface for a generic equity instrument\.
  
  + **Choice DeclareDividend**
    
    Declare a dividend distribution to shareholders\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Event identifier of the dividend distribution\.
       * - effectiveDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Date at which the dividend is distributed\.
       * - newInstrument
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-58546>`
         - Instrument held after the dividend distribution (ie\. \"ex\-dividend\" stock)\.
       * - perUnitDistribution
         - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-62956>`\]
         - Distributed quantities per unit held\.
  
  + **Choice DeclareReplacement**
    
    Declare a replacement event, where units of the instrument are replaced by a basket of other instruments\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Distribution Id\.
       * - effectiveDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Date the replacement is to be executed\.
       * - perUnitReplacement
         - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-62956>`\]
         - Payout offered to shareholders per held share\.
  
  + **Choice DeclareStockSplit**
    
    Declare a stock split\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Event identifier of the stock split\.
       * - effectiveDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Date at which the stock split is effective\.
       * - newInstrument
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-58546>`
         - Instrument to be held after the stock split is executed\.
       * - adjustmentFactor
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Adjustment factor for the stock split\. A factor of between 0 and 1 represents a classic stock split (eg\. 2\-for\-1 or two new for one old)\. A factor above 1 represents a reverse stock split (eg\. 1\-for\-2 or one new for two old)\.
  
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
  
  + **Method asInstrument \:** :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-67236>`
    
    Conversion to base ``Instrument`` interface\.
  
  + **Method declareDividend \:** DeclareDividend \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)
    
    Implementation fo the ``DeclareDividend`` choice\.
  
  + **Method declareReplacement \:** DeclareReplacement \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)
    
    Implementation fo the ``DeclareReplacement`` choice\.
  
  + **Method declareStockSplit \:** DeclareStockSplit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)
    
    Implementation fo the ``DeclareStockSplit`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-instrument-equity-instrument-hasimplementation-48374:

**class** `Implementation <type-daml-finance-interface-instrument-equity-instrument-implementation-8194_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-equity-instrument-hasimplementation-48374_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-instrument-equity-instrument-i-74160:

**type** `I <type-daml-finance-interface-instrument-equity-instrument-i-74160_>`_
  \= `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-99859_>`_

.. _type-daml-finance-interface-instrument-equity-instrument-implementation-8194:

**type** `Implementation <type-daml-finance-interface-instrument-equity-instrument-implementation-8194_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-instrument-equity-instrument-i-74160_>`_, :ref:`Implementation <type-daml-finance-interface-instrument-base-instrument-implementation-67110>` t)
  
  Type constraint used to require templates implementing ``Instrument`` to also implement ``BaseInstrument.I``\.

.. _type-daml-finance-interface-instrument-equity-instrument-v-19687:

**type** `V <type-daml-finance-interface-instrument-equity-instrument-v-19687_>`_
  \= `View <type-daml-finance-interface-instrument-equity-instrument-view-54781_>`_

.. _type-daml-finance-interface-instrument-equity-instrument-view-54781:

**data** `View <type-daml-finance-interface-instrument-equity-instrument-view-54781_>`_

  View for ``Instrument``\.
  
  .. _constr-daml-finance-interface-instrument-equity-instrument-view-37680:
  
  `View <constr-daml-finance-interface-instrument-equity-instrument-view-37680_>`_ ()
  
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-equity-instrument-view-54781_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-equity-instrument-view-54781_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-equity-instrument-view-54781_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-equity-instrument-asinstrument-86269:

`asInstrument <function-daml-finance-interface-instrument-equity-instrument-asinstrument-86269_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-99859_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-67236>`

.. _function-daml-finance-interface-instrument-equity-instrument-declaredividend-19644:

`declareDividend <function-daml-finance-interface-instrument-equity-instrument-declaredividend-19644_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-99859_>`_ \=\> t \-\> DeclareDividend \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)

.. _function-daml-finance-interface-instrument-equity-instrument-declarestocksplit-95101:

`declareStockSplit <function-daml-finance-interface-instrument-equity-instrument-declarestocksplit-95101_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-99859_>`_ \=\> t \-\> DeclareStockSplit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)

.. _function-daml-finance-interface-instrument-equity-instrument-declarereplacement-33010:

`declareReplacement <function-daml-finance-interface-instrument-equity-instrument-declarereplacement-33010_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-99859_>`_ \=\> t \-\> DeclareReplacement \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)
