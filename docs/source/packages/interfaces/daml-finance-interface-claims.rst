.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Claims.V4
################################

This package contains the *interface* for Contingent Claims based instruments. It contains the
following modules:

- :ref:`Claim <module-daml-finance-interface-claims-v4-claim-38573>`:
  Interface implemented by templates that can be represented as a set of contingent claims
- :ref:`Dynamic.Instrument <module-daml-finance-interface-claims-v4-dynamic-instrument-86702>`:
  Interface implemented by instruments that create Contingent Claims trees on-the-fly (i.e. the
  tree is not stored on disk as part of a contract, but created and processed in-memory)
- :ref:`Types <module-daml-finance-interface-claims-v4-types-7840>`:
  Types related to claims and what is required to represent claims (e.g. Deliverable and
  Observable)

Changelog
*********
