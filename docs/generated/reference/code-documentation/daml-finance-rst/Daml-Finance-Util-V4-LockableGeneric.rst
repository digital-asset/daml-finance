.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-util-v4-lockablegeneric-59737:

Daml.Finance.Util.V4.LockableGeneric
====================================

Functions
---------

.. _function-daml-finance-util-v4-lockablegeneric-acquireimplgeneric-38268:

`acquireImplGeneric <function-daml-finance-util-v4-lockablegeneric-acquireimplgeneric-38268_>`_
  \: (`HasCreate <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ t, `HasSignatory <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hassignatory-17507>`_ t, `HasFromInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`) \=\> `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> t) \-\> :ref:`Acquire <type-daml-finance-interface-util-v3-lockable-acquire-20270>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ t)

  Default implementation of ``acquire`` for the generic Lockable interface\.

.. _function-daml-finance-util-v4-lockablegeneric-releaseimplgeneric-88717:

`releaseImplGeneric <function-daml-finance-util-v4-lockablegeneric-releaseimplgeneric-88717_>`_
  \: (`HasCreate <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ t, `HasFromInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`) \=\> `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> t) \-\> :ref:`Release <type-daml-finance-interface-util-v3-lockable-release-15493>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ t)

  Default implementation of ``release`` for the generic Lockable interface\.

.. _function-daml-finance-util-v4-lockablegeneric-isvalidlockgeneric-63315:

`isValidLockGeneric <function-daml-finance-util-v4-lockablegeneric-isvalidlockgeneric-63315_>`_
  \: `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Check validity of lock\.
  The lockers field must be non\-empty if set\.
