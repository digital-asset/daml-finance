.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-core-v3-observation-36021:

ContingentClaims.Core.V3.Observation
====================================

Data Types
----------

.. _type-contingentclaims-core-v3-observation-observation-12406:

**data** `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o

  Implementation of market observables\.
  Conceptually it is helpful to think of this as the type ``t -> x``, or ``t -> Update x``\.

  .. _constr-contingentclaims-core-v3-observation-const-3622:

  `Const <constr-contingentclaims-core-v3-observation-const-3622_>`_

    A numerical constant, e\.g\. ``10.0``\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - value
         - x
         -

  .. _constr-contingentclaims-core-v3-observation-observe-27981:

  `Observe <constr-contingentclaims-core-v3-observation-observe-27981_>`_

    A named parameter, e\.g\. \"LIBOR 3M\"\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - key
         - o
         -

  .. _constr-contingentclaims-core-v3-observation-observeat-93788:

  `ObserveAt <constr-contingentclaims-core-v3-observation-observeat-93788_>`_

    A named parameter, e\.g\. \"LIBOR 3M\", observed at an explicit point in time\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - key
         - o
         -
       * - t
         - t
         -

  .. _constr-contingentclaims-core-v3-observation-add-94052:

  `Add <constr-contingentclaims-core-v3-observation-add-94052_>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o, `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

    Sum of two observations\.

  .. _constr-contingentclaims-core-v3-observation-neg-23251:

  `Neg <constr-contingentclaims-core-v3-observation-neg-23251_>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

    Opposite of an observation\.

  .. _constr-contingentclaims-core-v3-observation-mul-55491:

  `Mul <constr-contingentclaims-core-v3-observation-mul-55491_>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o, `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

    Product of two observations\.

  .. _constr-contingentclaims-core-v3-observation-div-92700:

  `Div <constr-contingentclaims-core-v3-observation-div-92700_>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o, `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

    Division of two observations\.

  **instance** Corecursive (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o) (ObservationF t x o)

  **instance** Recursive (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o) (ObservationF t x o)

  **instance** `Functor <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-base-functor-31205>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x)

  **instance** (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ x, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ o, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ t) \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

  **instance** `Additive <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-additive-25881>`_ x \=\> `Additive <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-additive-25881>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

  **instance** `Multiplicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-multiplicative-10593>`_ x \=\> `Divisible <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-divisible-86689>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

  **instance** `Multiplicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-multiplicative-10593>`_ x \=\> `Multiplicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-multiplicative-10593>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

  **instance** (`Additive <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-additive-25881>`_ x, `Multiplicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-multiplicative-10593>`_ x) \=\> `Number <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-number-53664>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

  **instance** (`Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ t, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ x, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ o) \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o)

Functions
---------

.. _function-contingentclaims-core-v3-observation-pure-70485:

`pure <function-contingentclaims-core-v3-observation-pure-70485_>`_
  \: x \-\> `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o

  Smart constructor for ``Const``\. Lifts a constant to an observation\.

.. _function-contingentclaims-core-v3-observation-observe-67896:

`observe <function-contingentclaims-core-v3-observation-observe-67896_>`_
  \: o \-\> `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o

  Smart constructor for ``Observe``\. Looks up the value of ``o``\.

.. _function-contingentclaims-core-v3-observation-eval-64097:

`eval <function-contingentclaims-core-v3-observation-eval-64097_>`_
  \: (`Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ t, `Number <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-number-53664>`_ x, `Divisible <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-divisible-86689>`_ x, `CanAbort <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-lf-canabort-29060>`_ m) \=\> (o \-\> t \-\> m x) \-\> `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x o \-\> t \-\> m x

  Reify the ``Observation`` into an observation function\.
  This function is used to convert an abstract observation, e\.g\. ``LIBOR 3M + 0.005`` to the actual
  observation function ``t -> m x``\.

.. _function-contingentclaims-core-v3-observation-mapparams-5210:

`mapParams <function-contingentclaims-core-v3-observation-mapparams-5210_>`_
  \: (i \-\> t) \-\> (o \-\> o') \-\> (x \-\> x') \-\> `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ i x o \-\> `Observation <type-contingentclaims-core-v3-observation-observation-12406_>`_ t x' o'

  The functor map operation *and* also map any parameters to keys\.
  For example, could map the param \"spot\" to an ISIN code \"GB123456789\"\.
  Also contra\-maps time parameter, i\.e\. from relative time values to absolute ones\.

  @ mapParams identity \= bimap
