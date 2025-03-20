.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-valuation-v0-stochastic-56833:

ContingentClaims.Valuation.V0.Stochastic
========================================

Typeclasses
-----------

.. _class-contingentclaims-valuation-v0-stochastic-isidentifier-37443:

**class** `IsIdentifier <class-contingentclaims-valuation-v0-stochastic-isidentifier-37443_>`_ t **where**

  .. _function-contingentclaims-valuation-v0-stochastic-localvar-89955:

  `localVar <function-contingentclaims-valuation-v0-stochastic-localvar-89955_>`_
    \: `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_ \-\> t

    Produce a local identifier of type ``t``, subindexed by ``i``\.

Data Types
----------

.. _type-contingentclaims-valuation-v0-stochastic-expr-82442:

**data** `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t

  Represents an expression of t\-adapted stochastic processes\.

  .. _constr-contingentclaims-valuation-v0-stochastic-const-71966:

  `Const <constr-contingentclaims-valuation-v0-stochastic-const-71966_>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_


  .. _constr-contingentclaims-valuation-v0-stochastic-ident-86121:

  `Ident <constr-contingentclaims-valuation-v0-stochastic-ident-86121_>`_ t


  .. _constr-contingentclaims-valuation-v0-stochastic-proc-56280:

  `Proc <constr-contingentclaims-valuation-v0-stochastic-proc-56280_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - name
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - process
         - `Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t
         -
       * - filtration
         - t
         -

  .. _constr-contingentclaims-valuation-v0-stochastic-sup-94161:

  `Sup <constr-contingentclaims-valuation-v0-stochastic-sup-94161_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lowerBound
         - t
         -
       * - tau
         - t
         -
       * - rv
         - `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t
         -

  .. _constr-contingentclaims-valuation-v0-stochastic-sum-92340:

  `Sum <constr-contingentclaims-valuation-v0-stochastic-sum-92340_>`_ \[`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t\]


  .. _constr-contingentclaims-valuation-v0-stochastic-neg-91387:

  `Neg <constr-contingentclaims-valuation-v0-stochastic-neg-91387_>`_ (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t)


  .. _constr-contingentclaims-valuation-v0-stochastic-mul-16891:

  `Mul <constr-contingentclaims-valuation-v0-stochastic-mul-16891_>`_ (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t, `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t)


  .. _constr-contingentclaims-valuation-v0-stochastic-pow-21779:

  `Pow <constr-contingentclaims-valuation-v0-stochastic-pow-21779_>`_ (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t, `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t)


  .. _constr-contingentclaims-valuation-v0-stochastic-i-81978:

  `I <constr-contingentclaims-valuation-v0-stochastic-i-81978_>`_ (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t, `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t)


  .. _constr-contingentclaims-valuation-v0-stochastic-e-99358:

  `E <constr-contingentclaims-valuation-v0-stochastic-e-99358_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - rv
         - `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t
         -
       * - filtration
         - t
         -

  **instance** :ref:`ToXml <class-contingentclaims-valuation-v0-mathml-toxml-72689>` t \=\> :ref:`ToXml <class-contingentclaims-valuation-v0-mathml-toxml-72689>` (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t)

  **instance** Corecursive (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t) (`ExprF <type-contingentclaims-valuation-v0-stochastic-exprf-29267_>`_ t)

  **instance** Recursive (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t) (`ExprF <type-contingentclaims-valuation-v0-stochastic-exprf-29267_>`_ t)

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ t \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t)

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ t \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t)

.. _type-contingentclaims-valuation-v0-stochastic-exprf-29267:

**data** `ExprF <type-contingentclaims-valuation-v0-stochastic-exprf-29267_>`_ t x

  Base functor for ``Expr``\. Note that this is ADT is re\-used in a couple of places, e\.g\.,
  ``Process``, where however not every choice is legal and will lead to a partial evaluator\.

  .. _constr-contingentclaims-valuation-v0-stochastic-constf-84247:

  `ConstF <constr-contingentclaims-valuation-v0-stochastic-constf-84247_>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_


  .. _constr-contingentclaims-valuation-v0-stochastic-identf-45154:

  `IdentF <constr-contingentclaims-valuation-v0-stochastic-identf-45154_>`_ t


  .. _constr-contingentclaims-valuation-v0-stochastic-procf-39361:

  `ProcF <constr-contingentclaims-valuation-v0-stochastic-procf-39361_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - name
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - process
         - `Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t
         -
       * - filtration
         - t
         -

  .. _constr-contingentclaims-valuation-v0-stochastic-supf-18986:

  `SupF <constr-contingentclaims-valuation-v0-stochastic-supf-18986_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lowerBound
         - t
         -
       * - tau
         - t
         -
       * - rv
         - x
         -

  .. _constr-contingentclaims-valuation-v0-stochastic-sumf-98637:

  `SumF <constr-contingentclaims-valuation-v0-stochastic-sumf-98637_>`_ \[x\]


  .. _constr-contingentclaims-valuation-v0-stochastic-negf-42352:

  `NegF <constr-contingentclaims-valuation-v0-stochastic-negf-42352_>`_ x


  .. _constr-contingentclaims-valuation-v0-stochastic-mulf-30384:

  `MulF <constr-contingentclaims-valuation-v0-stochastic-mulf-30384_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lhs
         - x
         -
       * - rhs
         - x
         -

  .. _constr-contingentclaims-valuation-v0-stochastic-powf-37144:

  `PowF <constr-contingentclaims-valuation-v0-stochastic-powf-37144_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lhs
         - x
         -
       * - rhs
         - x
         -

  .. _constr-contingentclaims-valuation-v0-stochastic-if-59875:

  `I_F <constr-contingentclaims-valuation-v0-stochastic-if-59875_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lhs
         - x
         -
       * - rhs
         - x
         -

  .. _constr-contingentclaims-valuation-v0-stochastic-ef-89015:

  `E_F <constr-contingentclaims-valuation-v0-stochastic-ef-89015_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - rv
         - x
         -
       * - filtration
         - t
         -

  **instance** Corecursive (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t) (`ExprF <type-contingentclaims-valuation-v0-stochastic-exprf-29267_>`_ t)

  **instance** Recursive (`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t) (`ExprF <type-contingentclaims-valuation-v0-stochastic-exprf-29267_>`_ t)

  **instance** `Functor <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-base-functor-31205>`_ (`ExprF <type-contingentclaims-valuation-v0-stochastic-exprf-29267_>`_ t)

  **instance** `Foldable <https://docs.daml.com/daml/stdlib/DA-Foldable.html#class-da-foldable-foldable-25994>`_ (`ExprF <type-contingentclaims-valuation-v0-stochastic-exprf-29267_>`_ t)

  **instance** `Traversable <https://docs.daml.com/daml/stdlib/DA-Traversable.html#class-da-traversable-traversable-18144>`_ (`ExprF <type-contingentclaims-valuation-v0-stochastic-exprf-29267_>`_ t)

.. _type-contingentclaims-valuation-v0-stochastic-process-65587:

**data** `Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t

  A stochastic processes\. Currently this represents a Geometric Browniam Motion, i\.e\.,
  dX / X \= α dt \+ β dW\. Eventually, we wish to support other processes such as Levy\.

  .. _constr-contingentclaims-valuation-v0-stochastic-process-63762:

  `Process <constr-contingentclaims-valuation-v0-stochastic-process-63762_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - dt
         - `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t
         -
       * - dW
         - `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ t \=\> `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ (`Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t)

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ t \=\> `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ (`Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t)

Functions
---------

.. _function-contingentclaims-valuation-v0-stochastic-riskless-1905:

`riskless <function-contingentclaims-valuation-v0-stochastic-riskless-1905_>`_
  \: t \-\> `Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t

  Helper function to create a riskless process ``dS = r dt``\.

.. _function-contingentclaims-valuation-v0-stochastic-gbm-47520:

`gbm <function-contingentclaims-valuation-v0-stochastic-gbm-47520_>`_
  \: t \-\> t \-\> `Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t

  Helper function to create a geometric BM ``dS = μ dt + σ dW``\.

.. _function-contingentclaims-valuation-v0-stochastic-fapf-50426:

`fapf <function-contingentclaims-valuation-v0-stochastic-fapf-50426_>`_
  \: (`Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ a, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ a, `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ o, `IsIdentifier <class-contingentclaims-valuation-v0-stochastic-isidentifier-37443_>`_ t) \=\> a \-\> (a \-\> `Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t) \-\> (a \-\> a \-\> `Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t) \-\> (o \-\> `Process <type-contingentclaims-valuation-v0-stochastic-process-65587_>`_ t) \-\> t \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ a o \-\> `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t

  Converts a ``Claim`` into the Fundamental Asset Pricing Formula\. The ϵ expressions are defined as
  E1\-E10 in the Eber/Peyton\-Jones paper\.
  This is still an experimental feature\.

.. _function-contingentclaims-valuation-v0-stochastic-simplify-87254:

`simplify <function-contingentclaims-valuation-v0-stochastic-simplify-87254_>`_
  \: `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t \-\> `Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442_>`_ t

  This is meant to be a function that algebraically simplifies the FAPF by

  1. using simple identities and ring laws
  2. change of numeraire technique\.
     This is still an experimental feature\.
