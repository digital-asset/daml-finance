.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-token-v4-types-62835:

Daml.Finance.Interface.Instrument.Token.V4.Types
================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-token-v4-types-token-51711:

**data** `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_

  Describes the attributes of a Token Instrument\.

  .. _constr-daml-finance-interface-instrument-token-v4-types-token-60206:

  `Token <constr-daml-finance-interface-instrument-token-v4-types-token-60206_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument's key\.
       * - description
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A description of the instrument\.
       * - validAsOf
         - `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Timestamp as of which the instrument is valid\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"token\" :ref:`Create <type-daml-finance-interface-instrument-token-v4-factory-create-20178>` `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"token\" :ref:`View <type-daml-finance-interface-instrument-token-v4-instrument-view-88163>` `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"validAsOf\" `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"token\" :ref:`Create <type-daml-finance-interface-instrument-token-v4-factory-create-20178>` `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"token\" :ref:`View <type-daml-finance-interface-instrument-token-v4-instrument-view-88163>` `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"validAsOf\" `Token <type-daml-finance-interface-instrument-token-v4-types-token-51711_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
