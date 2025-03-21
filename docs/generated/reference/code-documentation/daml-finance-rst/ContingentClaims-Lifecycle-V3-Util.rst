.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-lifecycle-v3-util-47746:

ContingentClaims.Lifecycle.V3.Util
==================================

This module defines a set of utility functions to extract information from claim trees\.

Functions
---------

.. _function-contingentclaims-lifecycle-v3-util-fixings-58971:

`fixings <function-contingentclaims-lifecycle-v3-util-fixings-58971_>`_
  \: :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> \[t\]

  Return the fixing dates of a claim\. This does not discriminate between optional dates which
  *may* result from a condition, and outright fixings\. It also does not correctly account for
  malformed trees, where subtrees are orphaned due to impossible ``When`` statements, e\.g\.,
  ``When (t > 1) ((When t < 1) _)``\.

.. _function-contingentclaims-lifecycle-v3-util-expiry-61667:

`expiry <function-contingentclaims-lifecycle-v3-util-expiry-61667_>`_
  \: `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ t \=\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ t

  Return the time after which the claim is worthless, i\.e\., value \= 0, if such a time exists\.
  Also known as 'maturity' or 'horizon' in the Eber/Jones paper\.

.. _function-contingentclaims-lifecycle-v3-util-payoffs-58129:

`payoffs <function-contingentclaims-lifecycle-v3-util-payoffs-58129_>`_
  \: (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ t, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ x, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ o, `Multiplicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-num-multiplicative-10593>`_ x) \=\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> \[(:ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o, a)\]

  Return a list of possible scale\-factor/payoff pairs\.
  This does not discriminate between conditional and outright payoffs\.

.. _function-contingentclaims-lifecycle-v3-util-prunezeros-98191:

`pruneZeros <function-contingentclaims-lifecycle-v3-util-prunezeros-98191_>`_
  \: :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o

  Prunes sub\-trees which are ``Zero``\.

.. _function-contingentclaims-lifecycle-v3-util-iszero-36536:

`isZero <function-contingentclaims-lifecycle-v3-util-iszero-36536_>`_
  \: :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Checks if a claim is the ``Zero`` claim\.
  This avoids requiring the equality type constraint on ``a``\.
