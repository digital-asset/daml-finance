.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-account-v4-util-78761:

Daml.Finance.Interface.Account.V4.Util
======================================

Functions
---------

.. _function-daml-finance-interface-account-v4-util-fetchaccount-79234:

`fetchAccount <function-daml-finance-interface-account-v4-util-fetchaccount-79234_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>` \=\> t \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ :ref:`I <type-daml-finance-interface-account-v4-account-i-22897>`

  Fetch the account of a holding\.

.. _function-daml-finance-interface-account-v4-util-getaccount-6084:

`getAccount <function-daml-finance-interface-account-v4-util-getaccount-6084_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>` \=\> t \-\> :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`

  Get the account key of a holding\.

.. _function-daml-finance-interface-account-v4-util-getcustodian-57495:

`getCustodian <function-daml-finance-interface-account-v4-util-getcustodian-57495_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>` \=\> t \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  Get the custodian of a holding\.

.. _function-daml-finance-interface-account-v4-util-getowner-12282:

`getOwner <function-daml-finance-interface-account-v4-util-getowner-12282_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>` \=\> t \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  Get the owner of a holding\.
