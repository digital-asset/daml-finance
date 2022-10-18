.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Contingent Claims
#################

.. toctree::
   :hidden:

   Quickstart <contingent-claims/quickstart>

Contingent Claims is a library for modeling contingent claims, i.e. derivatives,
written in the smart contract language
`Daml <https://www.digitalasset.com/developers>`__. Briefly, a
derivative is represented by a tree of ``Claim``\ s, which describe
future cashflows between two parties as well as the conditions under
which these cashflows occur.

The library offers life-cycling capabilities, as well as a valuation
semantics that maps a claim to a mathematical expression that can be
used for no-arbitrage pricing.

The implementation closely follows the model outlined in the papers [Cit1]_, [Cit2]_.

To get started, we recommend reading through the :doc:`quickstart <./contingent-claims/quickstart>` page.

Examples of how to create and lifecycle contracts can be found across the `Daml Finance` library.

References
==========

.. [Cit1] Jones, S. Peyton, Jean-Marc Eber, and Julian Seward. “Composing contracts: an adventure in financial engineering.” ACM SIG-PLAN Notices 35.9 (2000): 280-292.

.. [Cit2] Jones, SL Peyton, and J. M. Eber. “How to write a financial contract”, volume “Fun Of Programming” of “Cornerstones of Computing.” (2005).

The papers can be downloaded from `Microsoft
Research <https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/>`__.