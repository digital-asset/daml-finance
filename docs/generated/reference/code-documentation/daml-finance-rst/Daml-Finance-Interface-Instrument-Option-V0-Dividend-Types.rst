.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-dividend-types-32782:

Daml.Finance.Interface.Instrument.Option.V0.Dividend.Types
==========================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997:

**data** `Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997_>`_

  Describes the attributes of a physically settled Dividend option\.

  .. _constr-daml-finance-interface-instrument-option-v0-dividend-types-dividend-79286:

  `Dividend <constr-daml-finance-interface-instrument-option-v0-dividend-types-dividend-79286_>`_

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
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The description of the option\.
       * - expiryDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The expiry date of the option\.
       * - cashQuantity
         - :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
         - Dividend paid in cash
       * - sharesQuantity
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
         - Dividend paid in shares (if applicable)
       * - fxQuantity
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
         - Dividend paid in a foreign currency (if applicable)
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - prevEvents
         - \[EventData\]
         - A list of previous elections that have been lifecycled on this instrument so far\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997_>`_

.. _type-daml-finance-interface-instrument-option-v0-dividend-types-electiontypeenum-74590:

**data** `ElectionTypeEnum <type-daml-finance-interface-instrument-option-v0-dividend-types-electiontypeenum-74590_>`_

  An election type classification\.

  .. _constr-daml-finance-interface-instrument-option-v0-dividend-types-shares-60469:

  `Shares <constr-daml-finance-interface-instrument-option-v0-dividend-types-shares-60469_>`_

    Shares dividend\.

  .. _constr-daml-finance-interface-instrument-option-v0-dividend-types-cash-32962:

  `Cash <constr-daml-finance-interface-instrument-option-v0-dividend-types-cash-32962_>`_

    Cash dividend\.

  .. _constr-daml-finance-interface-instrument-option-v0-dividend-types-cashfx-20670:

  `CashFx <constr-daml-finance-interface-instrument-option-v0-dividend-types-cashfx-20670_>`_

    Foreign currency cash dividend\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ElectionTypeEnum <type-daml-finance-interface-instrument-option-v0-dividend-types-electiontypeenum-74590_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ElectionTypeEnum <type-daml-finance-interface-instrument-option-v0-dividend-types-electiontypeenum-74590_>`_
