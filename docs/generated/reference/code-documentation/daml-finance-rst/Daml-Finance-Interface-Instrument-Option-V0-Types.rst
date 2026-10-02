.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-types-54480:

Daml.Finance.Interface.Instrument.Option.V0.Types
=================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029:

**data** `BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029_>`_

  A barrier type classification\.

  .. _constr-daml-finance-interface-instrument-option-v0-types-upandout-80029:

  `UpAndOut <constr-daml-finance-interface-instrument-option-v0-types-upandout-80029_>`_

    The option is knocked out if the underlying trades at or above the barrier\.

  .. _constr-daml-finance-interface-instrument-option-v0-types-downandout-96532:

  `DownAndOut <constr-daml-finance-interface-instrument-option-v0-types-downandout-96532_>`_

    The option is knocked out if the underlying trades at or below the barrier\.

  .. _constr-daml-finance-interface-instrument-option-v0-types-upandin-16755:

  `UpAndIn <constr-daml-finance-interface-instrument-option-v0-types-upandin-16755_>`_

    The option is activated if the underlying trades at or above the barrier\.

  .. _constr-daml-finance-interface-instrument-option-v0-types-downandin-21676:

  `DownAndIn <constr-daml-finance-interface-instrument-option-v0-types-downandin-21676_>`_

    The option is activated if the underlying trades at or below the barrier\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierType\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierType\" :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>` `BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierType\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierType\" :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>` `BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029_>`_

.. _type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036:

**data** `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  An option type classification\.

  .. _constr-daml-finance-interface-instrument-option-v0-types-call-13259:

  `Call <constr-daml-finance-interface-instrument-option-v0-types-call-13259_>`_

    Call option\.

  .. _constr-daml-finance-interface-instrument-option-v0-types-put-54139:

  `Put <constr-daml-finance-interface-instrument-option-v0-types-put-54139_>`_

    Put option\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"optionType\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"optionType\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"optionType\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"optionType\" :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"optionType\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"optionType\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"optionType\" :ref:`Instrument <type-daml-finance-instrument-option-v0-barriereuropeancash-instrument-instrument-40010>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"optionType\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeancash-instrument-instrument-58340>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"optionType\" :ref:`Instrument <type-daml-finance-instrument-option-v0-europeanphysical-instrument-instrument-68822>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"optionType\" :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"optionType\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"optionType\" :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>` `OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036_>`_
