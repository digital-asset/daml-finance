.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

ContingentClaims.Core.V3
########################

This package contains the *interface* to represent
:doc:`Contingent Claims <../../instruments/generic/contingent-claims>` trees. It contains data types
and utility functions to process such trees. It also contains builder functions to facilitate the
creation of trees using composition. The following modules are included:

- :ref:`Builders <module-contingentclaims-core-v3-builders-35188>`: Builder functions to compose trees
  from smaller building blocks
- :ref:`Internal.Claim <module-contingentclaims-core-v3-internal-claim-26517>`: Internal data types to
  represent tree nodes
- :ref:`Claim <module-contingentclaims-core-v3-claim-98141>`: Smart constructors for the types defined
  in :ref:`Internal.Claim <module-contingentclaims-core-v3-internal-claim-26517>`
- :ref:`Observation <module-contingentclaims-core-v3-observation-36021>`: Data types to represent
  observations in trees
- :ref:`Util.Recursion <module-contingentclaims-core-v3-util-recursion-82116>`: Utility functions to
  facilitate recursive traversal of trees

Changelog
*********
