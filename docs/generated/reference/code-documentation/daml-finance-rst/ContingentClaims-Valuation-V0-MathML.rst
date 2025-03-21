.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-contingentclaims-valuation-v0-mathml-62715:

ContingentClaims.Valuation.V0.MathML
====================================

Typeclasses
-----------

.. _class-contingentclaims-valuation-v0-mathml-toxml-72689:

**class** `ToXml <class-contingentclaims-valuation-v0-mathml-toxml-72689_>`_ a **where**

  Renders an ``Expr`` into MathML presentation format\.

  .. _function-contingentclaims-valuation-v0-mathml-presentation-1921:

  `presentation <function-contingentclaims-valuation-v0-mathml-presentation-1921_>`_
    \: a \-\> Xml

  **instance** `ToXml <class-contingentclaims-valuation-v0-mathml-toxml-72689_>`_ t \=\> `ToXml <class-contingentclaims-valuation-v0-mathml-toxml-72689_>`_ (:ref:`Expr <type-contingentclaims-valuation-v0-stochastic-expr-82442>` t)

  **instance** `ToXml <class-contingentclaims-valuation-v0-mathml-toxml-72689_>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `ToXml <class-contingentclaims-valuation-v0-mathml-toxml-72689_>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `ToXml <class-contingentclaims-valuation-v0-mathml-toxml-72689_>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
