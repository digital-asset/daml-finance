.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-util-v4-lockable-69357:

Daml.Finance.Util.V4.Lockable
=============================

Functions
---------

.. _function-daml-finance-util-v4-lockable-acquireimpl-21150:

`acquireImpl <function-daml-finance-util-v4-lockable-acquireimpl-21150_>`_
  \: (`HasCreate <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ t, `HasSignatory <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hassignatory-17507>`_ t, `HasFromInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998>`, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998>`) \=\> `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150>` \-\> (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150>` \-\> t) \-\> :ref:`Acquire <type-daml-finance-interface-util-v3-lockablesplice-acquire-89216>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998>`)

  Default implementation of ``acquire`` from the ``Lockable`` interface\.

.. _function-daml-finance-util-v4-lockable-releaseimpl-47257:

`releaseImpl <function-daml-finance-util-v4-lockable-releaseimpl-47257_>`_
  \: (`HasCreate <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-template-functions-hascreate-45738>`_ t, `HasFromInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hasfrominterface-43863>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998>`, `HasToInterface <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998>`) \=\> `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150>` \-\> (`Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150>` \-\> t) \-\> :ref:`Release <type-daml-finance-interface-util-v3-lockablesplice-release-17663>` \-\> `Update <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Lockable <type-daml-finance-interface-util-v3-lockablesplice-lockable-7998>`)

  Default implementation of ``release`` from the ``Lockable`` interface\.

.. _function-daml-finance-util-v4-lockable-isvalidlock-2139:

`isValidLock <function-daml-finance-util-v4-lockable-isvalidlock-2139_>`_
  \: `Optional <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockablesplice-lock-9150>` \-\> `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Check validity of lock\.
  The lockers field must be non\-empty if set\.
