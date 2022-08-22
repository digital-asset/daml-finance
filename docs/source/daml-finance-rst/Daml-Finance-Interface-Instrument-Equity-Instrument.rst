.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-equity-instrument-89032:

Module Daml.Finance.Interface.Instrument.Equity.Instrument
===============================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-equity-instrument-instrument-81959:

**interface** `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_

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
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-75164>`
         - Instrument held after the dividend distribution (ie\. \"ex\-dividend\" stock)\.
       * - perUnitDistribution
         - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-31714>`\]
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
         - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-31714>`\]
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
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-75164>`
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

  + **Method asInstrument \:**\ :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-66474>`

    Conversion to base ``Instrument`` interface\.

  + **Method declareDividend \:**\ DeclareDividend \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)

    Implementation fo the ``DeclareDividend`` choice\.

  + **Method declareReplacement \:**\ DeclareReplacement \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)

    Implementation fo the ``DeclareReplacement`` choice\.

  + **Method declareStockSplit \:**\ DeclareStockSplit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)

    Implementation fo the ``DeclareStockSplit`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-instrument-equity-instrument-hasimplementation-6684:

**class** `Implementation <type-daml-finance-interface-instrument-equity-instrument-implementation-58678_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-equity-instrument-hasimplementation-6684_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-instrument-equity-instrument-hasimplementation-6684_>`_ :ref:`T <type-daml-finance-instrument-equity-instrument-t-33420>`

  **instance** `HasImplementation <class-daml-finance-interface-instrument-equity-instrument-hasimplementation-6684_>`_ `I <type-daml-finance-interface-instrument-equity-instrument-i-54484_>`_

Data Types
----------

.. _type-daml-finance-interface-instrument-equity-instrument-i-54484:

**type** `I <type-daml-finance-interface-instrument-equity-instrument-i-54484_>`_
  \= `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_

  **instance** `HasImplementation <class-daml-finance-interface-instrument-equity-instrument-hasimplementation-6684_>`_ `I <type-daml-finance-interface-instrument-equity-instrument-i-54484_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-equity-instrument-instrument-7660>` `I <type-daml-finance-interface-instrument-equity-instrument-i-54484_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-equity-instrument-instrument-7660>` `I <type-daml-finance-interface-instrument-equity-instrument-i-54484_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-instrument-equity-instrument-i-54484_>`_ :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-66474>`

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-instrument-equity-instrument-i-54484_>`_ :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _type-daml-finance-interface-instrument-equity-instrument-implementation-58678:

**type** `Implementation <type-daml-finance-interface-instrument-equity-instrument-implementation-58678_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-instrument-equity-instrument-i-54484_>`_, :ref:`Implementation <type-daml-finance-interface-instrument-base-instrument-implementation-94080>` t)

  Type constraint used to require templates implementing ``Instrument`` to also implement ``BaseInstrument.I``\.

.. _type-daml-finance-interface-instrument-equity-instrument-v-65251:

**type** `V <type-daml-finance-interface-instrument-equity-instrument-v-65251_>`_
  \= `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

.. _type-daml-finance-interface-instrument-equity-instrument-view-2217:

**data** `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

  View for ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-equity-instrument-view-85550:

  `View <constr-daml-finance-interface-instrument-equity-instrument-view-85550_>`_ ()


  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

  **instance** HasInterfaceView `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_ `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t GetView `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t GetView `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t GetView `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t GetView `View <type-daml-finance-interface-instrument-equity-instrument-view-2217_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-equity-instrument-asinstrument-42337:

`asInstrument <function-daml-finance-interface-instrument-equity-instrument-asinstrument-42337_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-66474>`

.. _function-daml-finance-interface-instrument-equity-instrument-declaredividend-32016:

`declareDividend <function-daml-finance-interface-instrument-equity-instrument-declaredividend-32016_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_ \=\> t \-\> DeclareDividend \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)

.. _function-daml-finance-interface-instrument-equity-instrument-declarestocksplit-88777:

`declareStockSplit <function-daml-finance-interface-instrument-equity-instrument-declarestocksplit-88777_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_ \=\> t \-\> DeclareStockSplit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)

.. _function-daml-finance-interface-instrument-equity-instrument-declarereplacement-72030:

`declareReplacement <function-daml-finance-interface-instrument-equity-instrument-declarereplacement-72030_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-81959_>`_ \=\> t \-\> DeclareReplacement \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`)
