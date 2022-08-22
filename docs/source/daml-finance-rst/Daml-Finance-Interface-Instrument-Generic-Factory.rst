.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-factory-5170:

Module Daml.Finance.Interface.Instrument.Generic.Factory
================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-generic-factory-factory-17847:

**interface** `Factory <type-daml-finance-interface-instrument-generic-factory-factory-17847_>`_

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
       * - claims
         - :ref:`C <type-daml-finance-interface-instrument-generic-types-c-63687>`
         - The claim tree\.
       * - acquisitionTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The claim's acquisition time\. This usually corresponds to the start date of the contract\.
       * - lastEventTimestamp
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

  + **Method asDisclosure \:**\ :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

    Conversion to ``Disclosure`` interface\.

  + **Method create' \:**\ Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`)

    Implementation of ``Create`` choice\.

  + **Method remove \:**\ Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

    Implementation of ``Remove`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-instrument-generic-factory-hasimplementation-48356:

**class** `Implementation <type-daml-finance-interface-instrument-generic-factory-implementation-37504_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-generic-factory-hasimplementation-48356_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-instrument-generic-factory-hasimplementation-48356_>`_ :ref:`Factory <type-daml-finance-instrument-generic-factory-factory-26064>`

  **instance** `HasImplementation <class-daml-finance-interface-instrument-generic-factory-hasimplementation-48356_>`_ `Factory <type-daml-finance-interface-instrument-generic-factory-factory-17847_>`_

Data Types
----------

.. _type-daml-finance-interface-instrument-generic-factory-f-50653:

**type** `F <type-daml-finance-interface-instrument-generic-factory-f-50653_>`_
  \= `Factory <type-daml-finance-interface-instrument-generic-factory-factory-17847_>`_

  Type synonym for ``Factory``\.

  **instance** `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ :ref:`Factory <type-daml-finance-instrument-generic-factory-factory-26064>` `F <type-daml-finance-interface-instrument-generic-factory-f-50653_>`_

  **instance** `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ :ref:`Factory <type-daml-finance-instrument-generic-factory-factory-26064>` `F <type-daml-finance-interface-instrument-generic-factory-f-50653_>`_

.. _type-daml-finance-interface-instrument-generic-factory-implementation-37504:

**type** `Implementation <type-daml-finance-interface-instrument-generic-factory-implementation-37504_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Factory <type-daml-finance-interface-instrument-generic-factory-factory-17847_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)

  Type constraint used to require templates implementing ``Factory`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-instrument-generic-factory-view-40435:

**data** `View <type-daml-finance-interface-instrument-generic-factory-view-40435_>`_

  .. _constr-daml-finance-interface-instrument-generic-factory-view-8398:

  `View <constr-daml-finance-interface-instrument-generic-factory-view-8398_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-generic-factory-view-40435_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-generic-factory-view-40435_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-generic-factory-view-40435_>`_

  **instance** HasInterfaceView `Factory <type-daml-finance-interface-instrument-generic-factory-factory-17847_>`_ `View <type-daml-finance-interface-instrument-generic-factory-view-40435_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-generic-factory-asdisclosure-58103:

`asDisclosure <function-daml-finance-interface-instrument-generic-factory-asdisclosure-58103_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-generic-factory-factory-17847_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-instrument-generic-factory-createtick-82602:

`create' <function-daml-finance-interface-instrument-generic-factory-createtick-82602_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-generic-factory-factory-17847_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`)

.. _function-daml-finance-interface-instrument-generic-factory-remove-15994:

`remove <function-daml-finance-interface-instrument-generic-factory-remove-15994_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-generic-factory-factory-17847_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
