.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-europeancash-types-91763:

Daml.Finance.Interface.Instrument.Option.V0.EuropeanCash.Types
==============================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694:

**data** `European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694_>`_

  Describes the attributes of a cash\-settled European option\.

  .. _constr-daml-finance-interface-instrument-option-v0-europeancash-types-european-7809:

  `European <constr-daml-finance-interface-instrument-option-v0-europeancash-types-european-7809_>`_

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
       * - referenceAssetId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The reference asset ID\. For example, in case of an option on AAPL this should be a valid reference to the AAPL fixings to be used for the payoff calculation\.
       * - ownerReceives
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - Indicate whether a holding owner of this instrument receives the option payoff\.
       * - optionType
         - :ref:`OptionTypeEnum <type-daml-finance-interface-instrument-option-v0-types-optiontypeenum-30036>`
         - Indicate whether the option is a call or a put\.
       * - strike
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The strike price of the option\.
       * - expiryDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The expiry date of the option\.
       * - currency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The currency of the option\. For example, if the option pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `European <type-daml-finance-interface-instrument-option-v0-europeancash-types-european-14694_>`_
