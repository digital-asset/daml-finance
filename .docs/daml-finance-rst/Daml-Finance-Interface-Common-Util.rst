.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-common-util-59853:

Module Daml.Finance.Interface.Common.Util
=========================================

Functions
---------

.. _function-daml-finance-interface-common-util-fetchinterfacebykey-11363:

`fetchInterfaceByKey <function-daml-finance-interface-common-util-fetchinterfacebykey-11363_>`_
  \: (`HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ t k, `HasField <https://docs.daml.com/daml/stdlib/DA-Record.html#class-da-internal-record-hasfield-52839>`_ \"cid\" t (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ i), `HasFetch <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetch-52387>`_ i) \=\> k \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ i
  
  Let us fetch an interface by key

.. _function-daml-finance-interface-common-util-verify-10454:

`verify <function-daml-finance-interface-common-util-verify-10454_>`_
  \: `CanAssert <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-assert-canassert-67323>`_ m \=\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> m ()
  
  Verify is assertMsg with its arguments flipped\.

.. _function-daml-finance-interface-common-util-flattenobservers-84840:

`flattenObservers <function-daml-finance-interface-common-util-flattenobservers-84840_>`_
  \: :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>` \-\> :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
  
  Flattens observers into a ``Set Party`` for usage in template definitions\. For example\:
  
  .. code-block:: daml
  
    observer $ flattenObservers observers
