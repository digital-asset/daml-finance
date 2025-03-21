.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-util-v3-common-32871:

Daml.Finance.Interface.Util.V3.Common
=====================================

Functions
---------

.. _function-daml-finance-interface-util-v3-common-verify-84388:

`verify <function-daml-finance-interface-util-v3-common-verify-84388_>`_
  \: `CanAssert <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-assert-canassert-67323>`_ m \=\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> m ()

  Verify is assertMsg with its arguments flipped\.

.. _function-daml-finance-interface-util-v3-common-qty-97062:

`qty <function-daml-finance-interface-util-v3-common-qty-97062_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>` \-\> :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`

  Wraps an amount and an instrument key into an instrument quantity\.

.. _function-daml-finance-interface-util-v3-common-scale-88838:

`scale <function-daml-finance-interface-util-v3-common-scale-88838_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>` \-\> :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`

  Scale ``quantity`` by the provided factor\.
