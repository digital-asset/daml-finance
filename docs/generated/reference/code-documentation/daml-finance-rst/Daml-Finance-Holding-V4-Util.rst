.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-holding-v4-util-71966:

Daml.Finance.Holding.V4.Util
============================

Functions
---------

.. _function-daml-finance-holding-v4-util-instrumentfromsplice-94155:

`instrumentFromSplice <function-daml-finance-holding-v4-util-instrumentfromsplice-94155_>`_
  \: InstrumentId \-\> :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

.. _function-daml-finance-holding-v4-util-metalookup-84791:

`metaLookup <function-daml-finance-holding-v4-util-metalookup-84791_>`_
  \: Metadata \-\> `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

.. _function-daml-finance-holding-v4-util-transferimpl-33631:

`transferImpl <function-daml-finance-holding-v4-util-transferimpl-33631_>`_
  \: :ref:`I <type-daml-finance-interface-holding-v4-transferable-i-68214>` \-\> `ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-transferable-i-68214>` \-\> :ref:`Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-transferable-i-68214>`)

  Default implementation of ``transfer`` for the ``Transferable`` interface\.

.. _function-daml-finance-holding-v4-util-splitimpl-38681:

`splitImpl <function-daml-finance-holding-v4-util-splitimpl-38681_>`_
  \: (`HasCreate <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ t, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-fungible-i-95581>`) \=\> :ref:`I <type-daml-finance-interface-holding-v4-fungible-i-95581>` \-\> (`Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> t) \-\> :ref:`Split <type-daml-finance-interface-holding-v4-fungible-split-16580>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ :ref:`SplitResult <type-daml-finance-interface-holding-v4-fungible-splitresult-97497>`

  Default implementation of ``split`` from the ``Fungible`` interface\.

.. _function-daml-finance-holding-v4-util-mergeimpl-70989:

`mergeImpl <function-daml-finance-holding-v4-util-mergeimpl-70989_>`_
  \: (`HasCreate <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ t, `HasArchive <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasarchive-7071>`_ t, `HasSignatory <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hassignatory-17507>`_ t, `HasFromInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ t :ref:`I <type-daml-finance-interface-holding-v4-fungible-i-95581>`, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-fungible-i-95581>`, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-util-v3-lockablesplice-i-56603>`) \=\> :ref:`I <type-daml-finance-interface-holding-v4-fungible-i-95581>` \-\> (t \-\> `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_) \-\> (`Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> t) \-\> :ref:`Merge <type-daml-finance-interface-holding-v4-fungible-merge-76684>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-fungible-i-95581>`)

  Default implementation of ``merge`` from the ``Fungible`` interface\.
