.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-holding-v4-util-74545:

Daml.Finance.Interface.Holding.V4.Util
======================================

Functions
---------

.. _function-daml-finance-interface-holding-v4-util-getinstrument-9221:

`getInstrument <function-daml-finance-interface-holding-v4-util-getinstrument-9221_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>` \=\> t \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  Get the key of a holding\.

.. _function-daml-finance-interface-holding-v4-util-getamount-39102:

`getAmount <function-daml-finance-interface-holding-v4-util-getamount-39102_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>` \=\> t \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Get the amount of a holding\.

.. _function-daml-finance-interface-holding-v4-util-disclose-59423:

`disclose <function-daml-finance-interface-holding-v4-util-disclose-59423_>`_
  \: (`HasInterfaceTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfacetyperep-84221>`_ i, `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>`, `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ i :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>`) \=\> (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`) \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>` \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i)

  Disclose a holding\.

.. _function-daml-finance-interface-holding-v4-util-undisclose-39552:

`undisclose <function-daml-finance-interface-holding-v4-util-undisclose-39552_>`_
  \: (`HasInterfaceTypeRep <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasinterfacetyperep-84221>`_ i, `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ i :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>`, `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ i :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>`) \=\> (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`) \-\> :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>` \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i))

  Undisclose a holding\.
