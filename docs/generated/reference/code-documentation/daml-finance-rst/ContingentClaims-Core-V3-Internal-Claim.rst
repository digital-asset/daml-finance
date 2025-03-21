.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-core-v3-internal-claim-26517:

ContingentClaims.Core.V3.Internal.Claim
=======================================

Data Types
----------

.. _type-contingentclaims-core-v3-internal-claim-claim-83050:

**data** `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o

  Core data type used to model cashflows of instruments\.
  In the reference paper from Peyton\-Jones this is called 'Contract'\.
  We renamed it to avoid ambiguity\.

  * ``t`` corresponds to the time parameter\.
  * ``x`` corresponds to the ``Observation`` output type\. An observation is a function from ``t`` to ``x``\.
    A common choice is to use ``Time`` and ``Decimal``, respectively\.
  * ``a`` is the representation of a deliverable asset, e\.g\. a ``Text`` ISIN code or
    an ``InstrumentKey``\.
  * ``o`` is the representation of an observable, e\.g\. a ``Text``\.

  You should build the ``Claim`` using the smart constructors (e\.g\. ``zero``, ``and``) instead of using
  the data constructors directly (``Zero``, ``And``)\.

  .. _constr-contingentclaims-core-v3-internal-claim-zero-31124:

  `Zero <constr-contingentclaims-core-v3-internal-claim-zero-31124_>`_

    Represents an absence of claims\. Monoid ``And`` identity\.

  .. _constr-contingentclaims-core-v3-internal-claim-one-23555:

  `One <constr-contingentclaims-core-v3-internal-claim-one-23555_>`_ a

    The bearer acquires one unit of ``a``  *immediately*\.

  .. _constr-contingentclaims-core-v3-internal-claim-give-76565:

  `Give <constr-contingentclaims-core-v3-internal-claim-give-76565_>`_ (`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o)

    The obligations of the bearer and the counterparty are reversed\.

  .. _constr-contingentclaims-core-v3-internal-claim-and-71128:

  `And <constr-contingentclaims-core-v3-internal-claim-and-71128_>`_

    Used to combine multiple rights together\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - a1
         - `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o
         -
       * - a2
         - `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o
         -
       * - as
         - \[`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o\]
         -

  .. _constr-contingentclaims-core-v3-internal-claim-or-27599:

  `Or <constr-contingentclaims-core-v3-internal-claim-or-27599_>`_

    Gives the bearer the right to choose between several claims\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - or1
         - `Electable <type-contingentclaims-core-v3-internal-claim-electable-24443_>`_ t x a o
         -
       * - or2
         - `Electable <type-contingentclaims-core-v3-internal-claim-electable-24443_>`_ t x a o
         -
       * - ors
         - \[`Electable <type-contingentclaims-core-v3-internal-claim-electable-24443_>`_ t x a o\]
         -

  .. _constr-contingentclaims-core-v3-internal-claim-cond-68070:

  `Cond <constr-contingentclaims-core-v3-internal-claim-cond-68070_>`_

    Gives the bearer the right to the first claim if ``predicate`` is true, else the second
    claim\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - predicate
         - `Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348_>`_ t x o
         -
       * - success
         - `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o
         -
       * - failure
         - `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o
         -

  .. _constr-contingentclaims-core-v3-internal-claim-scale-6591:

  `Scale <constr-contingentclaims-core-v3-internal-claim-scale-6591_>`_

    Multiplies the ``claim`` by ``k`` (which can be non\-deterministic)\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - k
         - :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o
         -
       * - claim
         - `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o
         -

  .. _constr-contingentclaims-core-v3-internal-claim-when-80674:

  `When <constr-contingentclaims-core-v3-internal-claim-when-80674_>`_

    Defers the acquisition of ``claim`` until *the first instant* that ``predicate`` is true\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - predicate
         - `Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348_>`_ t x o
         -
       * - claim
         - `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o
         -

  .. _constr-contingentclaims-core-v3-internal-claim-anytime-82510:

  `Anytime <constr-contingentclaims-core-v3-internal-claim-anytime-82510_>`_

    Gives the bearer the right to enter a claim at any time the predicate is true\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - predicate
         - `Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348_>`_ t x o
         -
       * - electable
         - `Electable <type-contingentclaims-core-v3-internal-claim-electable-24443_>`_ t x a o
         -

  .. _constr-contingentclaims-core-v3-internal-claim-until-16645:

  `Until <constr-contingentclaims-core-v3-internal-claim-until-16645_>`_

    Expires said claim on the *first instant* that ``predicate`` is true\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - predicate
         - `Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348_>`_ t x o
         -
       * - claim
         - `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o
         -

  **instance** Corecursive (`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o) (ClaimF t x a o)

  **instance** Recursive (`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o) (ClaimF t x a o)

  **instance** (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ x, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ o, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ t) \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o)

  **instance** (`Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ t, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ x, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ o) \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o)

  **instance** `Monoid <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-prelude-monoid-6742>`_ (`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o)

  **instance** `Semigroup <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-prelude-semigroup-78998>`_ (`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o)

.. _type-contingentclaims-core-v3-internal-claim-electable-24443:

**type** `Electable <type-contingentclaims-core-v3-internal-claim-electable-24443_>`_ t x a o
  \= (`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Claim <type-contingentclaims-core-v3-internal-claim-claim-83050_>`_ t x a o)

  Type synonym for sub\-trees that can be elected in an ``Or`` or ``Anytime`` node\.
  The textual tag is used to identify each sub\-tree when an election is made\.

.. _type-contingentclaims-core-v3-internal-claim-inequality-10348:

**data** `Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348_>`_ t x o

  Data type for boolean predicates supported by the library\.
  A boolean predicate is a generic function with signature ``t -> Bool``\. However, a limited
  set of predicates is currently supported\.

  .. _constr-contingentclaims-core-v3-internal-claim-timegte-43192:

  `TimeGte <constr-contingentclaims-core-v3-internal-claim-timegte-43192_>`_ t

    ``True`` when ``time ≥ t``, ``False`` otherwise\.

  .. _constr-contingentclaims-core-v3-internal-claim-timelte-59631:

  `TimeLte <constr-contingentclaims-core-v3-internal-claim-timelte-59631_>`_ t

    ``True`` when ``time ≤ t``, ``False`` otherwise\.

  .. _constr-contingentclaims-core-v3-internal-claim-lte-64386:

  `Lte <constr-contingentclaims-core-v3-internal-claim-lte-64386_>`_ (:ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o, :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` t x o)

    ``True`` when ``o(t) ≤ o'(t)``, ``False`` otherwise, for a pair of observations ``o``, ``o'``\.

  **instance** (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ t, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ x, `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ o) \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348_>`_ t x o)

  **instance** (`Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ t, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ x, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ o) \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Inequality <type-contingentclaims-core-v3-internal-claim-inequality-10348_>`_ t x o)
