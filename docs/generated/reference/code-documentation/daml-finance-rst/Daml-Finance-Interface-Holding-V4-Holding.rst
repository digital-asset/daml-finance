.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-holding-v4-holding-20535:

Daml.Finance.Interface.Holding.V4.Holding
=========================================

Interfaces
----------

.. _type-daml-finance-interface-holding-v4-holding-holding-66887:

**interface** `Holding <type-daml-finance-interface-holding-v4-holding-holding-66887_>`_

  Holding interface\.

  **viewtype** `V <type-daml-finance-interface-holding-v4-holding-v-1774_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-holding-v4-holding-getview-9417:

    **Choice** `GetView <type-daml-finance-interface-holding-v4-holding-getview-9417_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-holding-v4-holding-v-1774_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.


Data Types
----------

.. _type-daml-finance-interface-holding-v4-holding-i-25641:

**type** `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_
  \= `Holding <type-daml-finance-interface-holding-v4-holding-holding-66887_>`_

  Type synonym for ``Holding``\.

.. _type-daml-finance-interface-holding-v4-holding-v-1774:

**type** `V <type-daml-finance-interface-holding-v4-holding-v-1774_>`_
  \= `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Holding <type-daml-finance-interface-holding-v4-holding-holding-66887_>`_ `V <type-daml-finance-interface-holding-v4-holding-v-1774_>`_

.. _type-daml-finance-interface-holding-v4-holding-view-10906:

**data** `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_

  View for ``Holding``\.

  .. _constr-daml-finance-interface-holding-v4-holding-view-99999:

  `View <constr-daml-finance-interface-holding-v4-holding-view-99999_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - Instrument being held\.
       * - account
         - :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`
         - Key of the account holding the assets\.
       * - amount
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Size of the holding\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"account\" `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_ :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"amount\" `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"account\" `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_ :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"amount\" `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
