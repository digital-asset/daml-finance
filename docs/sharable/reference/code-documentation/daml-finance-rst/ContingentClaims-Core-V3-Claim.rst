.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-core-v3-claim-98141:

ContingentClaims.Core.V3.Claim
==============================

Functions
---------

.. _function-contingentclaims-core-v3-claim-zero-3849:

`zero <function-contingentclaims-core-v3-claim-zero-3849_>`_
  \: :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Constructs a claim without rights or obligations\.

.. _function-contingentclaims-core-v3-claim-one-90688:

`one <function-contingentclaims-core-v3-claim-one-90688_>`_
  \: a \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Constructs a claim that delivers one unit of ``a`` immediately to the bearer\.

.. _function-contingentclaims-core-v3-claim-give-33092:

`give <function-contingentclaims-core-v3-claim-give-33092_>`_
  \: :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Constructs a claim that reverses the obligations of the bearer and their counterparty\.

.. _function-contingentclaims-core-v3-claim-and-87779:

`and <function-contingentclaims-core-v3-claim-and-87779_>`_
  \: :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Used to additively combine two claims together\. In order to use this,
  you must import this module qualified or hide the ``and`` operator from ``Prelude``\.

.. _function-contingentclaims-core-v3-claim-or-29606:

`or <function-contingentclaims-core-v3-claim-or-29606_>`_
  \: :ref:`Electable <type-contingentclaims-core-v3-internal-claim-electable-24443>` t x a o \-\> :ref:`Electable <type-contingentclaims-core-v3-internal-claim-electable-24443>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Gives the bearer the right to choose between the input claims\. In order to use this,
  you must import this module qualified or hide the ``or`` operator from ``Prelude``\.

.. _function-contingentclaims-core-v3-claim-andlist-16715:

`andList <function-contingentclaims-core-v3-claim-andlist-16715_>`_
  \: \[:ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o\] \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Used to additively combine a list of claims together\. It is equivalent to
  applying the ``and`` builder recursively\.

.. _function-contingentclaims-core-v3-claim-orlist-42922:

`orList <function-contingentclaims-core-v3-claim-orlist-42922_>`_
  \: \[:ref:`Electable <type-contingentclaims-core-v3-internal-claim-electable-24443>` t x a o\] \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Gives the bearer the right to choose between the input claims\. It is equivalent to
  applying the ``or`` builder recursively\.

.. _function-contingentclaims-core-v3-claim-cond-30039:

`cond <function-contingentclaims-core-v3-claim-cond-30039_>`_
  \: :ref:`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348>` t x o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Gives the bearer the right to the first claim if predicate is true, else the second
  claim\.

.. _function-contingentclaims-core-v3-claim-scale-39304:

`scale <function-contingentclaims-core-v3-claim-scale-39304_>`_
  \: :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Multiplies the input claim by a scaling factor (which can be non\-deterministic)\.

.. _function-contingentclaims-core-v3-claim-when-40851:

`when <function-contingentclaims-core-v3-claim-when-40851_>`_
  \: :ref:`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348>` t x o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Acquires the input claim on *the first instant* that ``predicate`` is true\.

.. _function-contingentclaims-core-v3-claim-anytime-15949:

`anytime <function-contingentclaims-core-v3-claim-anytime-15949_>`_
  \: :ref:`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348>` t x o \-\> `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Gives the bearer the right to enter a claim at any time ``predicate`` is true\.

.. _function-contingentclaims-core-v3-claim-until-21738:

`until <function-contingentclaims-core-v3-claim-until-21738_>`_
  \: :ref:`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348>` t x o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Expires the input claim on the *first instant* that ``predicate`` is true\.

.. _function-contingentclaims-core-v3-claim-mapparams-95118:

`mapParams <function-contingentclaims-core-v3-claim-mapparams-95118_>`_
  \: (t \-\> i) \-\> (i \-\> t) \-\> (a \-\> a') \-\> (o \-\> o') \-\> (x \-\> x') \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` i x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x' a' o'

  Replaces parameters in a claims using the input mapping functions\.
  This can be used to e\.g\. map the time parameter in a claim from ``Date`` to ``Time``, or
  to map the asset type parameter from an abstract ``Text`` to a concrete ``InstrumentKey``\.

.. _function-contingentclaims-core-v3-claim-at-41106:

`at <function-contingentclaims-core-v3-claim-at-41106_>`_
  \: t \-\> :ref:`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348>` t x o

  Given ``t``, constructs a predicate that is ``True`` for time ≥ ``t``, ``False`` otherwise\.

.. _function-contingentclaims-core-v3-claim-upto-77107:

`upTo <function-contingentclaims-core-v3-claim-upto-77107_>`_
  \: t \-\> :ref:`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348>` t x a

  Given ``t``, constructs a predicate that is ``True`` for time ≤ ``t``, ``False`` otherwise\.

.. _function-contingentclaims-core-v3-claim-lteq-65404:

`(<=) <function-contingentclaims-core-v3-claim-lteq-65404_>`_
  \: :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o \-\> :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o \-\> :ref:`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348>` t x o

  Given observations ``o1`` and ``o2``, constructs the predicate ``o1 ≤ o2``\. In order to use this,
  you must import this module qualified or hide the ``(<=)`` operator from ``Prelude``\.

.. _function-contingentclaims-core-v3-claim-compare-90781:

`compare <function-contingentclaims-core-v3-claim-compare-90781_>`_
  \: (`Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ t, `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ x, `Number <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-number-53664>`_ x, `Divisible <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-divisible-86689>`_ x, `CanAbort <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-lf-canabort-29060>`_ m) \=\> (o \-\> t \-\> m x) \-\> :ref:`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348>` t x o \-\> t \-\> m `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Reify the ``Inequality`` into an observation function\.
  This function is used to convert an abstract predicate, e\.g\. ``S ≤ 50.0`` to the actual boolean
  observation function ``t -> m Bool``\.
