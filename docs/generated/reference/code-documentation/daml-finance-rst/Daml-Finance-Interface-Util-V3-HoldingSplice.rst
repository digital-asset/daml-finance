.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-util-v3-holdingsplice-31986:

Daml.Finance.Interface.Util.V3.HoldingSplice
============================================

Functions
---------

.. _function-daml-finance-interface-util-v3-holdingsplice-texttoparty-56399:

`textToParty <function-daml-finance-interface-util-v3-holdingsplice-texttoparty-56399_>`_
  \: `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  Convert a text back to a Party\. Inverse of partyToText\.

.. _function-daml-finance-interface-util-v3-holdingsplice-parseholdingstandard-14509:

`parseHoldingStandard <function-daml-finance-interface-util-v3-holdingsplice-parseholdingstandard-14509_>`_
  \: `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`

  Parse HoldingStandard from its textual form\.

.. _function-daml-finance-interface-util-v3-holdingsplice-getinstrument-68952:

`getInstrument <function-daml-finance-interface-util-v3-holdingsplice-getinstrument-68952_>`_
  \: Holding \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  Extract the ``InstrumentKey`` from a Holding, provided the appropriate metadata are populated\.

.. _function-daml-finance-interface-util-v3-holdingsplice-getamount-53259:

`getAmount <function-daml-finance-interface-util-v3-holdingsplice-getamount-53259_>`_
  \: Holding \-\> `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Extract the current token amount\.
