.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-core-v3-util-recursion-82116:

ContingentClaims.Core.V3.Util.Recursion
=======================================

This module collects a set of utilities used to execute recursion schemes\.
The morphisms ending in 'M' are monadic variants, allowing to interleave, e\.g\., ``Update`` or
``Script``\. ``cataM`` after Tim Williams' talk, https\://www\.youtube\.com/watch?v\=Zw9KeP3OzpU\.

Functions
---------

.. _function-contingentclaims-core-v3-util-recursion-param-97666:

`paraM <function-contingentclaims-core-v3-util-recursion-param-97666_>`_
  \: (Monad m, `Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ f, Recursive b f) \=\> (f (b, a) \-\> m a) \-\> b \-\> m a

  Monadic paramorphism\.

.. _function-contingentclaims-core-v3-util-recursion-anam-92357:

`anaM <function-contingentclaims-core-v3-util-recursion-anam-92357_>`_
  \: (Monad m, `Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ f, Corecursive b f) \=\> (a \-\> m (f a)) \-\> a \-\> m b

  Monadic anamorphism\.

.. _function-contingentclaims-core-v3-util-recursion-apom-39861:

`apoM <function-contingentclaims-core-v3-util-recursion-apom-39861_>`_
  \: (Monad m, `Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ f, Corecursive b f) \=\> (a \-\> m (f (`Either <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-types-either-56020>`_ b a))) \-\> a \-\> m b

  Monadic apomorphism\.

.. _function-contingentclaims-core-v3-util-recursion-futum-43934:

`futuM <function-contingentclaims-core-v3-util-recursion-futum-43934_>`_
  \: (Monad m, `Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ f, Corecursive b f) \=\> (a \-\> m (f (Free f a))) \-\> a \-\> m b

  Monadic futumorphism\.

.. _function-contingentclaims-core-v3-util-recursion-apocatam-92534:

`apoCataM <function-contingentclaims-core-v3-util-recursion-apocatam-92534_>`_
  \: (Monad m, `Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ f, Corecursive b f) \=\> (f b \-\> b) \-\> (a \-\> m (f (`Either <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-types-either-56020>`_ b a))) \-\> a \-\> m b

  Monadic lazy unfold (apoM) followed by a fold (cata)\.
  This Specialised lazy re\-fold is used by ``lifecycle``\.

.. _function-contingentclaims-core-v3-util-recursion-hylom-99842:

`hyloM <function-contingentclaims-core-v3-util-recursion-hylom-99842_>`_
  \: (`Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ f, Monad n) \=\> (f b \-\> b) \-\> (a \-\> n (f a)) \-\> a \-\> n b

  A modified ``hylo`` (refold), whith an interleaved monad effect (typically ``Update``)\.

.. _function-contingentclaims-core-v3-util-recursion-ghylom-43146:

`ghyloM <function-contingentclaims-core-v3-util-recursion-ghylom-43146_>`_
  \: (Comonad w, `Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ f, Monad m, `Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ m, Monad n) \=\> (f (w c) \-\> w (f c)) \-\> (m (f d) \-\> f (m d)) \-\> (f (w b) \-\> b) \-\> (a \-\> n (f (m a))) \-\> a \-\> n b

  Generalised hylomorphism (with monadic unfold)\.

.. _function-contingentclaims-core-v3-util-recursion-funzip-60224:

`funzip <function-contingentclaims-core-v3-util-recursion-funzip-60224_>`_
  \: `Functor <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-base-functor-31205>`_ f \=\> f (a, b) \-\> (f a, f b)

  Functor unzip\.

.. _function-contingentclaims-core-v3-util-recursion-synthesize-71760:

`synthesize <function-contingentclaims-core-v3-util-recursion-synthesize-71760_>`_
  \: (`Functor <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-base-functor-31205>`_ f, Recursive b f) \=\> (f attr \-\> attr) \-\> b \-\> Cofree f attr

  Annotate a recursive type bottom\-up\.

.. _function-contingentclaims-core-v3-util-recursion-inherit-91210:

`inherit <function-contingentclaims-core-v3-util-recursion-inherit-91210_>`_
  \: (`Functor <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-base-functor-31205>`_ f, Corecursive b f, Recursive b f) \=\> (b \-\> attr \-\> attr) \-\> attr \-\> b \-\> Cofree f attr

  Annotate a recursive type top\-down\.
