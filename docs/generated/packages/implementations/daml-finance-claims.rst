.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Claims.V3
######################

This package contains utility functions that facilitate building and working with
:doc:`Contingent Claims <../../instruments/generic/contingent-claims>` based instruments. It includes the
following modules:

- :ref:`Lifecycle.Rule <module-daml-finance-claims-v3-lifecycle-rule-196>`:
  Rule to process a lifecycle event for instruments that build a
  :doc:`Contingent Claims tree dynamically <../../tutorials/advanced-topics/instrument-modeling/contingent-claims-instrument>`.
- :ref:`Util <module-daml-finance-claims-v3-util-10150>`:
  Contains utility functions for claims, e.g., checking the content of a claim and converting the
  claim time
- :ref:`Util.Lifecycle <module-daml-finance-claims-v3-util-lifecycle-43238>`:
  Defines different types of events and how to lifecycle them
- :ref:`Util.Builders <module-daml-finance-claims-v3-util-builders-30825>`:
  Utility functions related to creating :doc:`Contingent Claims <../../instruments/generic/contingent-claims>`,
  e.g. for bonds/swaps
- :ref:`Util.Date <module-daml-finance-claims-v3-util-date-8229>`:
  Utility functions related to dates and schedule periods, which are used to define claims.

Changelog
*********
