.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-util-v4-common-85309:

Daml.Finance.Util.V4.Common
===========================

Functions
---------

.. _function-daml-finance-util-v4-common-notnull-64516:

`notNull <function-daml-finance-util-v4-common-notnull-64516_>`_
  \: \[a\] \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Checks if the input list is not empty\.

.. _function-daml-finance-util-v4-common-sortandgroupon-91538:

`sortAndGroupOn <function-daml-finance-util-v4-common-sortandgroupon-91538_>`_
  \: `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ k \=\> (a \-\> k) \-\> \[a\] \-\> \[\[a\]\]

  Like ``List.groupOn``, but sorts the list first\.
