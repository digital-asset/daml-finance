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

.. _function-daml-finance-interface-util-v3-holdingsplice-parseholdingstandard-14509:

`parseHoldingStandard <function-daml-finance-interface-util-v3-holdingsplice-parseholdingstandard-14509_>`_
  \: `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`

  Parse HoldingStandard from its textual form

.. _function-daml-finance-interface-util-v3-holdingsplice-getinstrument-68952:

`getInstrument <function-daml-finance-interface-util-v3-holdingsplice-getinstrument-68952_>`_
  \: `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t Holding \=\> t \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

.. _function-daml-finance-interface-util-v3-holdingsplice-getamount-53259:

`getAmount <function-daml-finance-interface-util-v3-holdingsplice-getamount-53259_>`_
  \: `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t Holding \=\> t \-\> `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

.. _function-daml-finance-interface-util-v3-holdingsplice-disclose-47904:

`disclose <function-daml-finance-interface-util-v3-holdingsplice-disclose-47904_>`_
  \: (`HasInterfaceTypeRep <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfacetyperep-84221>`_ i, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>`, `HasFromInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ i :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>`) \=\> (`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`) \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>` \-\> `ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i)

.. _function-daml-finance-interface-util-v3-holdingsplice-undisclose-79163:

`undisclose <function-daml-finance-interface-util-v3-holdingsplice-undisclose-79163_>`_
  \: (`HasInterfaceTypeRep <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfacetyperep-84221>`_ i, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>`, `HasFromInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ i :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>`) \=\> (`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`) \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>` \-\> `ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i))
