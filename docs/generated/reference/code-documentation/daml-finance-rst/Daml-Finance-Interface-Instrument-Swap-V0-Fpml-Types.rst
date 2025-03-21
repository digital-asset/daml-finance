.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-fpml-types-85330:

Daml.Finance.Interface.Instrument.Swap.V0.Fpml.Types
====================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949:

**data** `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-79074:

  `Fpml <constr-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-79074_>`_

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
         - The description of the swap\.
       * - swapStreams
         - \[:ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>`\]
         - Each element describes a stream of swap payments, for example a regular fixed or floating rate\.
       * - issuerPartyRef
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Used to the identify which counterparty is the issuer in the swapStream\.
       * - calendarDataProvider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The reference data provider to use for the holiday calendar\.
       * - currencies
         - \[:ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`\]
         - The currencies of the different swap legs, one for each swapStream\. For example, if one leg pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Fpml <type-daml-finance-interface-instrument-swap-v0-fpml-types-fpml-35949_>`_
