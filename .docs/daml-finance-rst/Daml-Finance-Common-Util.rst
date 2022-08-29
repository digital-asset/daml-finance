.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-common-util-73870:

Module Daml.Finance.Common.Util
===============================

Functions
---------

.. _function-daml-finance-common-util-groupby-76707:

`groupBy <function-daml-finance-common-util-groupby-76707_>`_
  \: `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ k \=\> (a \-\> k) \-\> \[a\] \-\> `Map <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ k \[a\]
  
  Like ``List.groupOn``, but returns the output in a ``Map``\.

.. _function-daml-finance-common-util-mapwithindex-10118:

`mapWithIndex <function-daml-finance-common-util-mapwithindex-10118_>`_
  \: (a \-\> `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_ \-\> b) \-\> \[a\] \-\> \[b\]
  
  Like ``map``, but the mapping function includes the zero\-based index of the item\.

.. _function-daml-finance-common-util-notnull-55645:

`notNull <function-daml-finance-common-util-notnull-55645_>`_
  \: \[a\] \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
  
  Checks if the input list is not empty\.
