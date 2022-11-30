.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

ContingentClaims.Valuation
##########################

This package contains the *implementation* of utility functions to map a
:doc:`Contingent Claims <../../concepts/contingent-claims>` tree into a mathematical representation
to facilitate integration with pricing and risk frameworks. The following modules are included:

- :ref:`Stochastic <module-contingentclaims-valuation-stochastic-37844>`: Utilities to map a
  :doc:`Contingent Claims <../../concepts/contingent-claims>` tree to a stochastic process
  representation
- :ref:`MathML <module-contingentclaims-valuation-mathml-30102>`: Typeclass definition to map an
  expression in the MathML presentation format