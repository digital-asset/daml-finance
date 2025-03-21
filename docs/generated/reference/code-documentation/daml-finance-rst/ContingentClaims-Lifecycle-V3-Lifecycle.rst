.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-lifecycle-v3-lifecycle-61551:

ContingentClaims.Lifecycle.V3.Lifecycle
=======================================

Data Types
----------

.. _type-contingentclaims-lifecycle-v3-lifecycle-pending-58851:

**data** `Pending <type-contingentclaims-lifecycle-v3-lifecycle-pending-58851_>`_ t a

  Used to specify pending payments\.

  .. _constr-contingentclaims-lifecycle-v3-lifecycle-pending-38284:

  `Pending <constr-contingentclaims-lifecycle-v3-lifecycle-pending-38284_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - t
         - t
         - Payment time\.
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Amount of asset to be paid\.
       * - asset
         - a
         - Asset in which the payment is denominated\.

  **instance** (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ t, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a) \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Pending <type-contingentclaims-lifecycle-v3-lifecycle-pending-58851_>`_ t a)

  **instance** (`Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ t, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a) \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Pending <type-contingentclaims-lifecycle-v3-lifecycle-pending-58851_>`_ t a)

.. _type-contingentclaims-lifecycle-v3-lifecycle-result-98744:

**data** `Result <type-contingentclaims-lifecycle-v3-lifecycle-result-98744_>`_ t a o

  Returned from a ``lifecycle`` operation\.

  .. _constr-contingentclaims-lifecycle-v3-lifecycle-result-1445:

  `Result <constr-contingentclaims-lifecycle-v3-lifecycle-result-1445_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - pending
         - \[`Pending <type-contingentclaims-lifecycle-v3-lifecycle-pending-58851_>`_ t a\]
         - Payments requiring settlement\.
       * - remaining
         - C t a o
         - The tree after lifecycled branches have been pruned\.

  **instance** (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ o, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ t) \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Result <type-contingentclaims-lifecycle-v3-lifecycle-result-98744_>`_ t a o)

  **instance** (`Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ t, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ o) \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Result <type-contingentclaims-lifecycle-v3-lifecycle-result-98744_>`_ t a o)

Functions
---------

.. _function-contingentclaims-lifecycle-v3-lifecycle-lifecycle-11948:

`lifecycle <function-contingentclaims-lifecycle-v3-lifecycle-lifecycle-11948_>`_
  \: (`Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ t, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a, `CanAbort <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-lf-canabort-29060>`_ m) \=\> (o \-\> t \-\> m `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_) \-\> C t a o \-\> t \-\> t \-\> m (`Result <type-contingentclaims-lifecycle-v3-lifecycle-result-98744_>`_ t a o)

  Collect claims falling due into a list, and return the tree with those nodes pruned\.
  ``m`` will typically be ``Update``\. It is parametrised so it can be run in a ``Script``\. The first
  argument is used to lookup the value of any ``Observables``\. Returns the pruned tree \+ pending
  settlements up to the provided market time\.

.. _function-contingentclaims-lifecycle-v3-lifecycle-exercise-91455:

`exercise <function-contingentclaims-lifecycle-v3-lifecycle-exercise-91455_>`_
  \: (`Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ t, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ o, `CanAbort <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-lf-canabort-29060>`_ m) \=\> (o \-\> t \-\> m `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_) \-\> (`Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_) \-\> C t a o \-\> t \-\> t \-\> m (C t a o)

  Acquire ``Anytime`` and ``Or`` nodes, by making an election\.
  Import this ``qualified`` to avoid clashes with ``Prelude.exercise``\.
