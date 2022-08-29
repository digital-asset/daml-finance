.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-instrument-27802:

Module Daml.Finance.Interface.Asset.Instrument
==============================================

Interfaces
----------

.. _type-daml-finance-interface-asset-instrument-instrument-30765:

**interface** `Instrument <type-daml-finance-interface-asset-instrument-instrument-30765_>`_

  Base interface for an instrument\. This interface does not define any lifecycling logic\.
  
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
  
  + **Method asDisclosure \: **:ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
    
    Conversion to ``Disclosure`` interface\.
  
  + **Method getKey \: **:ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
    
    Get the unique key for the ``Instrument``\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-instrument-hasimplementation-51108:

**class** `Implementation <type-daml-finance-interface-asset-instrument-implementation-94080_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-instrument-hasimplementation-51108_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-asset-instrument-i-66474:

**type** `I <type-daml-finance-interface-asset-instrument-i-66474_>`_
  \= `Instrument <type-daml-finance-interface-asset-instrument-instrument-30765_>`_
  
  Type synonym for ``Instrument``\.
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-asset-factory-instrument-factory-88339>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-instrument-i-66474_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-bond-fixedrate-factory-94553>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-instrument-i-66474_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-bond-floatingrate-factory-88424>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-instrument-i-66474_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-bond-inflationlinked-factory-99998>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-instrument-i-66474_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-bond-zerocoupon-factory-77382>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-instrument-i-66474_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-derivative-factory-factory-17847>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-instrument-i-66474_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-equity-factory-factory-50265>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-instrument-i-66474_>`_))
  
  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-equity-instrument-instrument-81959>` \"asInstrument\" `I <type-daml-finance-interface-asset-instrument-i-66474_>`_

.. _type-daml-finance-interface-asset-instrument-implementation-94080:

**type** `Implementation <type-daml-finance-interface-asset-instrument-implementation-94080_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-asset-instrument-i-66474_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Instrument`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-asset-instrument-k-75164:

**type** `K <type-daml-finance-interface-asset-instrument-k-75164_>`_
  \= :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
  
  Type synonym for ``InstrumentKey``\.

.. _type-daml-finance-interface-asset-instrument-q-31714:

**type** `Q <type-daml-finance-interface-asset-instrument-q-31714_>`_
  \= :ref:`Quantity <type-daml-finance-interface-asset-types-quantity-64806>` :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
  
  Type synonym for ``Quantity``\.

.. _type-daml-finance-interface-asset-instrument-r-42481:

**type** `R <type-daml-finance-interface-asset-instrument-r-42481_>`_
  \= Reference
  
  Type synonym for ``Reference``\. This type is currently used as a work\-around given the lack of interface keys\.

.. _type-daml-finance-interface-asset-instrument-v-25101:

**type** `V <type-daml-finance-interface-asset-instrument-v-25101_>`_
  \= `View <type-daml-finance-interface-asset-instrument-view-18387_>`_
  
  Type synonym for ``View``\.

.. _type-daml-finance-interface-asset-instrument-view-18387:

**data** `View <type-daml-finance-interface-asset-instrument-view-18387_>`_

  View for ``Instrument``\.
  
  .. _constr-daml-finance-interface-asset-instrument-view-17750:
  
  `View <constr-daml-finance-interface-asset-instrument-view-17750_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - issuer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The instrument's issuer\.
       * - depository
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The instrument's depository\.
       * - id
         - :ref:`Id <type-daml-finance-interface-asset-types-id-89116>`
         - An instrument identifier\. It includes a textual label as well as a textual version\.
       * - validAsOf
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Timestamp as of which the instrument is valid\. This usually coincides with the timestamp of the event that creates the instrument\. It usually does not coincide with ledger time\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-instrument-view-18387_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-asset-instrument-view-18387_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-instrument-view-18387_>`_

Functions
---------

.. _function-daml-finance-interface-asset-instrument-exerciseinterfacebykey-31595:

`exerciseInterfaceByKey <function-daml-finance-interface-asset-instrument-exerciseinterfacebykey-31595_>`_
  \: `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t2 d r \=\> `K <type-daml-finance-interface-asset-instrument-k-75164_>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> d \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ r
  
  Exercise interface by key\.
  This method can be used to exercise a choice on an ``Instrument`` given its ``InstrumentKey``\.
  Requires as input the ``InstrumentKey``, the actor fetching the instrument and the choice arguments\. For example\:

.. _function-daml-finance-interface-asset-instrument-asdisclosure-98127:

`asDisclosure <function-daml-finance-interface-asset-instrument-asdisclosure-98127_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-asset-instrument-instrument-30765_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-asset-instrument-getkey-4735:

`getKey <function-daml-finance-interface-asset-instrument-getkey-4735_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-asset-instrument-instrument-30765_>`_ \=\> t \-\> :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`

.. _function-daml-finance-interface-asset-instrument-tokey-1447:

`toKey <function-daml-finance-interface-asset-instrument-tokey-1447_>`_
  \: `View <type-daml-finance-interface-asset-instrument-view-18387_>`_ \-\> :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
  
  Convert the instrument's View to its key\.

.. _function-daml-finance-interface-asset-instrument-qty-13389:

`qty <function-daml-finance-interface-asset-instrument-qty-13389_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>` \-\> :ref:`Quantity <type-daml-finance-interface-asset-types-quantity-64806>` :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
  
  Wraps an amount and an instrument key into an instrument quantity\.

.. _function-daml-finance-interface-asset-instrument-scale-34989:

`scale <function-daml-finance-interface-asset-instrument-scale-34989_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Quantity <type-daml-finance-interface-asset-types-quantity-64806>` :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Quantity <type-daml-finance-interface-asset-types-quantity-64806>` :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
  
  Scale ``Quantity`` by the provided factor\.
