.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-equity-lifecycle-corporateaction-28127:

Module Daml.Finance.Interface.Equity.Lifecycle.CorporateAction
==============================================================

Interfaces
----------

.. _type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853:

**interface** `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_

  An interface for a generic corporate action\.

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

  + **Method asEvent \:**\ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

    Conversion to ``Event`` interface\.

  + **Method asLifecyclable \:**\ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

    Conversion to ``Lifecyclable`` interface\.

  + **Method view \:**\ `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

    Acquire the default interface view\.

Typeclasses
-----------

.. _class-daml-finance-interface-equity-lifecycle-corporateaction-hasimplementation-15367:

**class** `Implementation <type-daml-finance-interface-equity-lifecycle-corporateaction-implementation-89565_>`_ t \=\> `HasImplementation <class-daml-finance-interface-equity-lifecycle-corporateaction-hasimplementation-15367_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-equity-lifecycle-corporateaction-hasimplementation-15367_>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_

Data Types
----------

.. _type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005:

**type** `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_
  \= `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_

  **instance** `HasImplementation <class-daml-finance-interface-equity-lifecycle-corporateaction-hasimplementation-15367_>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_

  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \"distributionImpl\" (Distribution \-\> :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_))

  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \"replacementImpl\" (Replacement \-\> :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_))

  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \"stockSplitImpl\" (StockSplit \-\> :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_))

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Distribution <type-daml-finance-equity-lifecycle-distribution-distribution-95534>` `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Replacement <type-daml-finance-equity-lifecycle-replacement-replacement-69986>` `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`StockSplit <type-daml-finance-equity-lifecycle-stocksplit-stocksplit-51182>` `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Distribution <type-daml-finance-equity-lifecycle-distribution-distribution-95534>` `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Replacement <type-daml-finance-equity-lifecycle-replacement-replacement-69986>` `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`StockSplit <type-daml-finance-equity-lifecycle-stocksplit-stocksplit-51182>` `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>`) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t Distribution (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>`) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t Replacement (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>`) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t StockSplit (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>`) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t Distribution (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>`) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t Replacement (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>`) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t StockSplit (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t Distribution (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t Replacement (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t StockSplit (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t Distribution (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t Replacement (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t StockSplit (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_)

.. _type-daml-finance-interface-equity-lifecycle-corporateaction-implementation-89565:

**type** `Implementation <type-daml-finance-interface-equity-lifecycle-corporateaction-implementation-89565_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005_>`_, :ref:`Implementation <type-daml-finance-interface-lifecycle-lifecyclable-implementation-10942>` t, :ref:`Implementation <type-daml-finance-interface-lifecycle-event-implementation-22192>` t)

  Type constraint used to require templates implementing ``CorporateAction`` to also
  implement ``Lifecyclable.I`` and ``Event.I``

.. _type-daml-finance-interface-equity-lifecycle-corporateaction-v-49338:

**type** `V <type-daml-finance-interface-equity-lifecycle-corporateaction-v-49338_>`_
  \= `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

.. _type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326:

**data** `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

  View for ``CorporateAction``\.

  .. _constr-daml-finance-interface-equity-lifecycle-corporateaction-view-18063:

  `View <constr-daml-finance-interface-equity-lifecycle-corporateaction-view-18063_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - offerer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party which offers the Corporate Action to investors\.
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Equity lifecycle identifier\.
       * - effectiveDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Date when the lifecycle is to be executed\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

  **instance** HasMethod `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_ \"view\" `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t GetView `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t GetView `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t GetView `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t GetView `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

Functions
---------

.. _function-daml-finance-interface-equity-lifecycle-corporateaction-view-29090:

`view <function-daml-finance-interface-equity-lifecycle-corporateaction-view-29090_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_ \=\> t \-\> `View <type-daml-finance-interface-equity-lifecycle-corporateaction-view-22326_>`_

.. _function-daml-finance-interface-equity-lifecycle-corporateaction-aslifecyclable-69544:

`asLifecyclable <function-daml-finance-interface-equity-lifecycle-corporateaction-aslifecyclable-69544_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

.. _function-daml-finance-interface-equity-lifecycle-corporateaction-asevent-26594:

`asEvent <function-daml-finance-interface-equity-lifecycle-corporateaction-asevent-26594_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `CorporateAction <type-daml-finance-interface-equity-lifecycle-corporateaction-corporateaction-52853_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`
