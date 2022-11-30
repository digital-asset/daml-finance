.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

ContingentClaims.Core
#####################

This package contains the *interface* to represent
:doc:`Contingent Claims <../../concepts/contingent-claims>` trees. It contains data types and
utility functions to process such trees. It also contains builder functions to facilitate the
creation of trees using composition. The following modules are included:

- :ref:`Builders <module-contingentclaims-core-builders-15004>`: Builder functions to compose trees
  from smaller building blocks
- :ref:`Internal.Claim <module-contingentclaims-core-internal-claim-23633>`: Internal data types to
  represent tree nodes.
- :ref:`Claim <module-contingentclaims-core-claim-90861>`: Smart constructors for the types defined
  in :ref:`Internal.Claim <module-contingentclaims-core-internal-claim-23633>`
- :ref:`Observation <module-contingentclaims-core-observation-86605>`: Data types to represent
  observation in trees.
- :ref:`Util.Recursion <module-contingentclaims-core-util-recursion-31812>`: Utility functions to
  facilitate recursive traversal of trees.
