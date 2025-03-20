.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-util-v4-lockable-69357:

Daml.Finance.Util.V4.Lockable
=============================

Functions
---------

.. _function-daml-finance-util-v4-lockable-acquireimpl-21150:

`acquireImpl <function-daml-finance-util-v4-lockable-acquireimpl-21150_>`_
  \: (`HasCreate <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ t, `HasSignatory <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hassignatory-17507>`_ t, `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`, `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`) \=\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> t) \-\> :ref:`Acquire <type-daml-finance-interface-util-v3-lockable-acquire-20270>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`)

  Default implementation of ``acquire`` from the ``Lockable`` interface\.

.. _function-daml-finance-util-v4-lockable-releaseimpl-47257:

`releaseImpl <function-daml-finance-util-v4-lockable-releaseimpl-47257_>`_
  \: (`HasCreate <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ t, `HasFromInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`, `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`) \=\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> t) \-\> :ref:`Release <type-daml-finance-interface-util-v3-lockable-release-15493>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Lockable <type-daml-finance-interface-util-v3-lockable-lockable-79556>`)

  Default implementation of ``release`` from the ``Lockable`` interface\.

.. _function-daml-finance-util-v4-lockable-isvalidlock-2139:

`isValidLock <function-daml-finance-util-v4-lockable-isvalidlock-2139_>`_
  \: `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Check validity of lock\.
  The lockers field must be non\-empty if set\.
