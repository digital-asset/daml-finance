.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-common-disclosure-6626:

Module Daml.Finance.Interface.Common.Disclosure
===============================================

Interfaces
----------

.. _type-daml-finance-interface-common-disclosure-disclosure-75793:

**interface** `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_

  An interface for managing the visibility of contracts for non\-authorizing parties\.

  + **Choice AddObservers**

    Add a single new observer context to the existing observers\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - disclosers
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Party calling this choice\.
       * - observersToAdd
         - (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`)
         - Observer context to add to a contract\.

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

  + **Choice RemoveObservers**

    Remove an observer context from the existing observers\.
    Any party can undisclose itself\. None is returned if no update is needed\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - observersToRemove
         - (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`)
         - Observer context to remove\.

  + **Choice SetObservers**

    Set the observers for a contract\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - disclosers
         - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Party calling this choice\.
       * - newObservers
         - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
         - Observers to set for this contract\. This overrides the existing observers\.

  + **Method archive' \:**\ `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

    Implemetation of archiving the contract\.

  + **Method setObservers \:**\ SetObservers \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_)

    Implementation of the ``SetObservers`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-common-disclosure-hasimplementation-11898:

**class** `Implementation <type-daml-finance-interface-common-disclosure-implementation-6532_>`_ t \=\> `HasImplementation <class-daml-finance-interface-common-disclosure-hasimplementation-11898_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-common-disclosure-hasimplementation-11898_>`_ `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasImplementation <class-daml-finance-interface-common-disclosure-hasimplementation-11898_>`_ T

Data Types
----------

.. _type-daml-finance-interface-common-disclosure-i-70158:

**type** `I <type-daml-finance-interface-common-disclosure-i-70158_>`_
  \= `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_

  Type synonym for ``Disclosure``\.

  **instance** `HasImplementation <class-daml-finance-interface-common-disclosure-hasimplementation-11898_>`_ `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Account <type-daml-finance-interface-asset-account-account-19971>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-asset-factory-account-factory-23412>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-asset-factory-holding-factory-96220>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Holding <type-daml-finance-interface-asset-holding-holding-42619>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-asset-instrument-instrument-30765>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-94553>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-zerocoupon-factory-77382>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-generic-factory-factory-17847>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-equity-factory-factory-50265>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** HasMethod :ref:`Instruction <type-daml-finance-interface-settlement-instruction-instruction-30569>` \"asDisclosure\" `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Account <type-daml-finance-asset-account-account-64286>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-asset-account-factory-10857>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-asset-fungible-factory-36455>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Fungible <type-daml-finance-asset-fungible-fungible-62518>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-asset-instrument-factory-1836>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-asset-instrument-instrument-84320>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-asset-nonfungible-factory-66887>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`NonFungible <type-daml-finance-asset-nonfungible-nonfungible-15366>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-asset-nontransferable-factory-91184>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`NonTransferable <type-daml-finance-asset-nontransferable-nontransferable-14506>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-instrument-bond-fixedrate-factory-54926>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-bond-fixedrate-instrument-1982>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-instrument-bond-floatingrate-factory-2361>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-bond-floatingrate-instrument-41475>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-instrument-bond-inflationlinked-factory-12125>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-bond-inflationlinked-instrument-28311>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-instrument-bond-zerocoupon-factory-71815>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-bond-zerocoupon-instrument-49917>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Election <type-daml-finance-generic-election-election-47518>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-generic-factory-factory-26064>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-generic-instrument-instrument-92650>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-instrument-equity-factory-factory-66762>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instrument <type-daml-finance-instrument-equity-instrument-instrument-7660>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Observation <type-daml-finance-refdata-observation-observation-39199>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`DateClock <type-daml-finance-refdata-time-dateclock-dateclock-68517>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Account <type-daml-finance-asset-account-account-64286>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-asset-account-factory-10857>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-asset-fungible-factory-36455>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Fungible <type-daml-finance-asset-fungible-fungible-62518>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-asset-instrument-factory-1836>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-asset-instrument-instrument-84320>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-asset-nonfungible-factory-66887>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`NonFungible <type-daml-finance-asset-nonfungible-nonfungible-15366>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-asset-nontransferable-factory-91184>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`NonTransferable <type-daml-finance-asset-nontransferable-nontransferable-14506>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-instrument-bond-fixedrate-factory-54926>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-bond-fixedrate-instrument-1982>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-instrument-bond-floatingrate-factory-2361>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-bond-floatingrate-instrument-41475>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-instrument-bond-inflationlinked-factory-12125>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-bond-inflationlinked-instrument-28311>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-instrument-bond-zerocoupon-factory-71815>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-bond-zerocoupon-instrument-49917>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Election <type-daml-finance-generic-election-election-47518>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-generic-factory-factory-26064>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-generic-instrument-instrument-92650>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-instrument-equity-factory-factory-66762>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instrument <type-daml-finance-instrument-equity-instrument-instrument-7660>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`I <type-daml-finance-interface-asset-account-i-38237>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`F <type-daml-finance-interface-asset-factory-account-f-54942>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`F <type-daml-finance-interface-asset-factory-holding-f-78374>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`I <type-daml-finance-interface-asset-fungible-i-30537>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`I <type-daml-finance-interface-asset-lockable-i-23182>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`I <type-daml-finance-interface-asset-transferable-i-10374>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-94553>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-interface-instrument-bond-zerocoupon-factory-77382>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-interface-generic-factory-factory-17847>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-interface-instrument-equity-factory-factory-50265>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`I <type-daml-finance-interface-instrument-equity-instrument-i-54484>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`I <type-daml-finance-interface-settlement-instruction-i-90342>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Observation <type-daml-finance-refdata-observation-observation-39199>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`DateClock <type-daml-finance-refdata-time-dateclock-dateclock-68517>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Instruction <type-daml-finance-settlement-instruction-instruction-35758>` `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

.. _type-daml-finance-interface-common-disclosure-implementation-6532:

**type** `Implementation <type-daml-finance-interface-common-disclosure-implementation-6532_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-common-disclosure-i-70158_>`_

  Type constraint used to require templates implementing ``Disclosure`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-common-disclosure-view-17247:

**data** `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

  View for ``Disclosure``\.

  .. _constr-daml-finance-interface-common-disclosure-view-34892:

  `View <constr-daml-finance-interface-common-disclosure-view-34892_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - disclosureControllers
         - `Set <https://docs.daml.com/daml/stdlib/DA-Set.html#type-da-set-types-set-90436>`_ :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
         - Disjunction choice controllers\.
       * - observers
         - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
         - Observers with context\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

  **instance** HasInterfaceView `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_ `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_) \=\> `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t GetView `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

  **instance** (HasIsInterfaceType t, `HasTemplateTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastemplatetyperep-24134>`_ t, `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_) \=\> `HasExerciseGuarded <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexerciseguarded-97843>`_ t GetView `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_ \=\> `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ t GetView `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

  **instance** `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_ \=\> `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ t GetView `View <type-daml-finance-interface-common-disclosure-view-17247_>`_

Functions
---------

.. _function-daml-finance-interface-common-disclosure-setobservers-25577:

`setObservers <function-daml-finance-interface-common-disclosure-setobservers-25577_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_ \=\> t \-\> SetObservers \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_)

.. _function-daml-finance-interface-common-disclosure-archivetick-70829:

`archive' <function-daml-finance-interface-common-disclosure-archivetick-70829_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_ \=\> t \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Disclosure <type-daml-finance-interface-common-disclosure-disclosure-75793_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
