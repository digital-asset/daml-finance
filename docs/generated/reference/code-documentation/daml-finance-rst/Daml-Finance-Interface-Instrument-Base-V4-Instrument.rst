.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-base-v4-instrument-47185:

Daml.Finance.Interface.Instrument.Base.V4.Instrument
====================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494:

**interface** `Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494_>`_

  Base interface for all instruments\. This interface does not define any lifecycling logic\.

  **viewtype** `V <type-daml-finance-interface-instrument-base-v4-instrument-v-55368_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-base-v4-instrument-getview-66559:

    **Choice** `GetView <type-daml-finance-interface-instrument-base-v4-instrument-getview-66559_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-instrument-base-v4-instrument-remove-35281:

    **Choice** `Remove <type-daml-finance-interface-instrument-base-v4-instrument-remove-35281_>`_

    Archive the instrument\.

    Controller\: signatory this

    Returns\: ()

    (no fields)

  + **Method getKey \:** :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

    Get the unique key for the ``Instrument``\.

Data Types
----------

.. _type-daml-finance-interface-instrument-base-v4-instrument-i-70415:

**type** `I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415_>`_
  \= `Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494_>`_

  Type synonym for ``Instrument``\.

.. _type-daml-finance-interface-instrument-base-v4-instrument-q-66135:

**type** `Q <type-daml-finance-interface-instrument-base-v4-instrument-q-66135_>`_
  \= :ref:`Quantity <type-daml-finance-interface-types-common-v3-types-quantity-28585>` :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>` `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Instrument quantity\.

.. _type-daml-finance-interface-instrument-base-v4-instrument-r-72748:

**type** `R <type-daml-finance-interface-instrument-base-v4-instrument-r-72748_>`_
  \= Reference

  Type synonym for ``Reference``\. This type is currently used as a work\-around given the lack of
  interface keys\.

.. _type-daml-finance-interface-instrument-base-v4-instrument-v-55368:

**type** `V <type-daml-finance-interface-instrument-base-v4-instrument-v-55368_>`_
  \= `View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494_>`_ `V <type-daml-finance-interface-instrument-base-v4-instrument-v-55368_>`_

.. _type-daml-finance-interface-instrument-base-v4-instrument-view-52900:

**data** `View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900_>`_

  View for ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-base-v4-instrument-view-91959:

  `View <constr-daml-finance-interface-instrument-base-v4-instrument-view-91959_>`_

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
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - The instrument's identifier\.
       * - version
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual instrument version\.
       * - holdingStandard
         - :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`
         - The holding standard for holdings referencing this instrument\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A human readable description of the instrument\.
       * - validAsOf
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Timestamp as of which the instrument is valid\. This usually coincides with the timestamp of the event that creates the instrument\. It usually does not coincide with ledger time\. This is required for lifecycling of some instruments, in order to keep track of the last time the instrument was lifecycled\. For instruments where this is not applicable, it can be set to the current time\.

  **instance** HasInterfaceKey `Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494_>`_ `View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>` Reference GetCid SetCid SetObservers `GetView <type-daml-finance-interface-instrument-base-v4-instrument-getview-66559_>`_

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-base-v4-instrument-view-52900_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-base-v4-instrument-tokey-84902:

`toKey <function-daml-finance-interface-instrument-base-v4-instrument-tokey-84902_>`_
  \: `V <type-daml-finance-interface-instrument-base-v4-instrument-v-55368_>`_ \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  Convert the instrument's View to its key\.

.. _function-daml-finance-interface-instrument-base-v4-instrument-instrumentkey-25474:

`instrumentKey <function-daml-finance-interface-instrument-base-v4-instrument-instrumentkey-25474_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i `Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494_>`_ \=\> i \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  Retrieves the key of an ``Instrument``\.

.. _function-daml-finance-interface-instrument-base-v4-instrument-getkey-72348:

`getKey <function-daml-finance-interface-instrument-base-v4-instrument-getkey-72348_>`_
  \: `Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494_>`_ \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

.. _function-daml-finance-interface-instrument-base-v4-instrument-qty-30556:

`qty <function-daml-finance-interface-instrument-base-v4-instrument-qty-30556_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>` \-\> `Q <type-daml-finance-interface-instrument-base-v4-instrument-q-66135_>`_

  Wraps an amount and an instrument key into an instrument quantity\.

.. _function-daml-finance-interface-instrument-base-v4-instrument-scale-95016:

`scale <function-daml-finance-interface-instrument-base-v4-instrument-scale-95016_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Q <type-daml-finance-interface-instrument-base-v4-instrument-q-66135_>`_ \-\> `Q <type-daml-finance-interface-instrument-base-v4-instrument-q-66135_>`_

  Scale ``Quantity`` by the provided factor\.

.. _function-daml-finance-interface-instrument-base-v4-instrument-fetchinstrument-72799:

`fetchInstrument <function-daml-finance-interface-instrument-base-v4-instrument-fetchinstrument-72799_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>` \=\> t \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Instrument <type-daml-finance-interface-instrument-base-v4-instrument-instrument-74494_>`_

  Fetch instrument from holding\.

.. _function-daml-finance-interface-instrument-base-v4-instrument-exerciseinterfacebykey-98720:

`exerciseInterfaceByKey <function-daml-finance-interface-instrument-base-v4-instrument-exerciseinterfacebykey-98720_>`_
  \: (`HasInterfaceTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfacetyperep-84221>`_ i, `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ i c r) \=\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> c \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ r

  Exercise interface by key\.
  This method can be used to exercise a choice on an ``Instrument`` given its ``InstrumentKey``\.
  Requires as input the ``InstrumentKey``, the actor exercising the choice, and the choice arguments\.
