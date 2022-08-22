.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-hasclaims-95591:

Module Daml.Finance.Interface.Instrument.Generic.HasClaims
==================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955:

**interface** `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_

  Interface implemented by templates that admit a representation as a set of contingent claims\.

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

  + **Method getClaims \:**\ `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\]

    Gets the set of claims representing the instrument\.

Data Types
----------

.. _type-daml-finance-interface-instrument-generic-hasclaims-i-90893:

**type** `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_
  \= `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_

  **instance** HasImplementation `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-bond-fixedrate-instrument-1982>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-bond-floatingrate-instrument-41475>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-bond-inflationlinked-instrument-28311>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-bond-zerocoupon-instrument-49917>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-generic-instrument-instrument-92650>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-bond-fixedrate-instrument-1982>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-bond-floatingrate-instrument-41475>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-bond-inflationlinked-instrument-28311>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-bond-zerocoupon-instrument-49917>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-generic-instrument-instrument-92650>` `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

.. _type-daml-finance-interface-instrument-generic-hasclaims-implementation-84525:

**type** `Implementation <type-daml-finance-interface-instrument-generic-hasclaims-implementation-84525_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893_>`_

  Type constraint used to require templates implementing ``HasClaims`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-instrument-generic-hasclaims-v-32266:

**type** `V <type-daml-finance-interface-instrument-generic-hasclaims-v-32266_>`_
  \= `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

.. _type-daml-finance-interface-instrument-generic-hasclaims-view-80326:

**data** `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

  View for ``HasClaims``\.

  .. _constr-daml-finance-interface-instrument-generic-hasclaims-view-70023:

  `View <constr-daml-finance-interface-instrument-generic-hasclaims-view-70023_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - acquisitionTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The claim's acquisition time\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

  **instance** HasInterfaceView `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_ `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t GetView `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t GetView `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t GetView `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t GetView `View <type-daml-finance-interface-instrument-generic-hasclaims-view-80326_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-generic-hasclaims-getclaims-42355:

`getClaims <function-daml-finance-interface-instrument-generic-hasclaims-getclaims-42355_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_ \=\> t \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\]

.. _function-daml-finance-interface-instrument-generic-hasclaims-getclaim-12249:

`getClaim <function-daml-finance-interface-instrument-generic-hasclaims-getclaim-12249_>`_
  \: `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ :ref:`C <type-daml-finance-interface-instrument-generic-types-c-63687>`

  Retrieves the single claim representing the template\. An error is thrown if there are zero or more than one claims\.

.. _function-daml-finance-interface-instrument-generic-hasclaims-getacquisitiontime-51329:

`getAcquisitionTime <function-daml-finance-interface-instrument-generic-hasclaims-getacquisitiontime-51329_>`_
  \: `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955_>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  Retrieves the claim's acquisition time\.
