.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-base-instrument-57320:

Module Daml.Finance.Interface.Instrument.Base.Instrument
========================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-base-instrument-instrument-22935:

**interface** `Instrument <type-daml-finance-interface-instrument-base-instrument-instrument-22935_>`_

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
  
  + **Method asDisclosure \:** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
    
    Conversion to ``Disclosure`` interface\.
  
  + **Method getKey \:** :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>`
    
    Get the unique key for the ``Instrument``\.

Typeclasses
-----------

.. _class-daml-finance-interface-instrument-base-instrument-hasimplementation-37642:

**class** `Implementation <type-daml-finance-interface-instrument-base-instrument-implementation-67110_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-base-instrument-hasimplementation-37642_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-instrument-base-instrument-i-67236:

**type** `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_
  \= `Instrument <type-daml-finance-interface-instrument-base-instrument-instrument-22935_>`_
  
  Type synonym for ``Instrument``\.
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-base-factory-factory-67517>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-27717>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-71700>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-67758>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-zerocoupon-factory-76014>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-equity-factory-factory-60517>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_))
  
  **instance** HasMethod :ref:`Instrument <type-daml-finance-interface-instrument-equity-instrument-instrument-99859>` \"asInstrument\" `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-generic-factory-factory-64962>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_))

.. _type-daml-finance-interface-instrument-base-instrument-implementation-67110:

**type** `Implementation <type-daml-finance-interface-instrument-base-instrument-implementation-67110_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Instrument`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-instrument-base-instrument-k-58546:

**type** `K <type-daml-finance-interface-instrument-base-instrument-k-58546_>`_
  \= :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>`
  
  Type synonym for ``InstrumentKey``\.

.. _type-daml-finance-interface-instrument-base-instrument-q-62956:

**type** `Q <type-daml-finance-interface-instrument-base-instrument-q-62956_>`_
  \= :ref:`Quantity <type-daml-finance-interface-common-types-quantity-934>` :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
  
  Type synonym for ``Quantity``\.

.. _type-daml-finance-interface-instrument-base-instrument-r-56343:

**type** `R <type-daml-finance-interface-instrument-base-instrument-r-56343_>`_
  \= Reference
  
  Type synonym for ``Reference``\. This type is currently used as a work\-around given the lack of interface keys\.

.. _type-daml-finance-interface-instrument-base-instrument-v-38963:

**type** `V <type-daml-finance-interface-instrument-base-instrument-v-38963_>`_
  \= `View <type-daml-finance-interface-instrument-base-instrument-view-86425_>`_
  
  Type synonym for ``View``\.

.. _type-daml-finance-interface-instrument-base-instrument-view-86425:

**data** `View <type-daml-finance-interface-instrument-base-instrument-view-86425_>`_

  View for ``Instrument``\.
  
  .. _constr-daml-finance-interface-instrument-base-instrument-view-82976:
  
  `View <constr-daml-finance-interface-instrument-base-instrument-view-82976_>`_
  
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
         - :ref:`Id <type-daml-finance-interface-common-types-id-88316>`
         - An instrument identifier\. It includes a textual label as well as a textual version\.
       * - validAsOf
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Timestamp as of which the instrument is valid\. This usually coincides with the timestamp of the event that creates the instrument\. It usually does not coincide with ledger time\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-base-instrument-view-86425_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-base-instrument-view-86425_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-base-instrument-view-86425_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-base-instrument-exerciseinterfacebykey-27297:

`exerciseInterfaceByKey <function-daml-finance-interface-instrument-base-instrument-exerciseinterfacebykey-27297_>`_
  \: `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ t2 d r \=\> `K <type-daml-finance-interface-instrument-base-instrument-k-58546_>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> d \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ r
  
  Exercise interface by key\.
  This method can be used to exercise a choice on an ``Instrument`` given its ``InstrumentKey``\.
  Requires as input the ``InstrumentKey``, the actor fetching the instrument and the choice arguments\. For example\:

.. _function-daml-finance-interface-instrument-base-instrument-asdisclosure-6717:

`asDisclosure <function-daml-finance-interface-instrument-base-instrument-asdisclosure-6717_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-base-instrument-instrument-22935_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-instrument-base-instrument-getkey-81449:

`getKey <function-daml-finance-interface-instrument-base-instrument-getkey-81449_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Instrument <type-daml-finance-interface-instrument-base-instrument-instrument-22935_>`_ \=\> t \-\> :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>`

.. _function-daml-finance-interface-instrument-base-instrument-tokey-12561:

`toKey <function-daml-finance-interface-instrument-base-instrument-tokey-12561_>`_
  \: `View <type-daml-finance-interface-instrument-base-instrument-view-86425_>`_ \-\> :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>`
  
  Convert the instrument's View to its key\.

.. _function-daml-finance-interface-instrument-base-instrument-fetchinstrument-77672:

`fetchInstrument <function-daml-finance-interface-instrument-base-instrument-fetchinstrument-77672_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` \=\> t \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `I <type-daml-finance-interface-instrument-base-instrument-i-67236_>`_
  
  Fetch instrument from holding\.

.. _function-daml-finance-interface-instrument-base-instrument-qty-82779:

`qty <function-daml-finance-interface-instrument-base-instrument-qty-82779_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>` \-\> :ref:`Quantity <type-daml-finance-interface-common-types-quantity-934>` :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
  
  Wraps an amount and an instrument key into an instrument quantity\.

.. _function-daml-finance-interface-instrument-base-instrument-scale-11943:

`scale <function-daml-finance-interface-instrument-base-instrument-scale-11943_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Quantity <type-daml-finance-interface-common-types-quantity-934>` :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Quantity <type-daml-finance-interface-common-types-quantity-934>` :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
  
  Scale ``Quantity`` by the provided factor\.
