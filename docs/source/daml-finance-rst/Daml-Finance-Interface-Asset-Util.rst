.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-util-84617:

Module Daml.Finance.Interface.Asset.Util
========================================

Functions
---------

.. _function-daml-finance-interface-asset-util-fetchaccount-1506:

`fetchAccount <function-daml-finance-interface-asset-util-fetchaccount-1506_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` \=\> t \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ :ref:`I <type-daml-finance-interface-asset-account-i-38237>`

  Fetch the account of a holding\.

.. _function-daml-finance-interface-asset-util-fetchinstrument-22695:

`fetchInstrument <function-daml-finance-interface-asset-util-fetchinstrument-22695_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` \=\> t \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-66474>`

  Fetch instrument from holding\.

.. _function-daml-finance-interface-asset-util-getinstrument-33161:

`getInstrument <function-daml-finance-interface-asset-util-getinstrument-33161_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` \=\> t \-\> :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`

  Get the instrument key of a holding\.

.. _function-daml-finance-interface-asset-util-getaccount-52132:

`getAccount <function-daml-finance-interface-asset-util-getaccount-52132_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` \=\> t \-\> :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`

  Get the account key of a holding\.

.. _function-daml-finance-interface-asset-util-getcustodian-28151:

`getCustodian <function-daml-finance-interface-asset-util-getcustodian-28151_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` \=\> t \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  Get the custodian of a holding\.

.. _function-daml-finance-interface-asset-util-getowner-19546:

`getOwner <function-daml-finance-interface-asset-util-getowner-19546_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` \=\> t \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  Get the owner of a holding\.

.. _function-daml-finance-interface-asset-util-getamount-77482:

`getAmount <function-daml-finance-interface-asset-util-getamount-77482_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-holding-i-4221>` \=\> t \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Get the amount of a holding\.

.. _function-daml-finance-interface-asset-util-getlocker-10358:

`getLocker <function-daml-finance-interface-asset-util-getlocker-10358_>`_
  \: `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t :ref:`I <type-daml-finance-interface-asset-lockable-i-23182>` \=\> t \-\> :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`

  Get the lockers of a lockable holding\.
