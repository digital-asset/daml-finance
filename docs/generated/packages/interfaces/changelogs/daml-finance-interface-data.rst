.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Data.V4
==============================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Data
===========================

Version 3.1.0
*************

- Update of SDK version and dependencies.

- Added `I` as type synonym for each `Factory` in the package (the `F` type synonyms are to be
  deprecated).

Version 3.0.0
*************

- Update of SDK version and dependencies.

- Remove implementation of `Remove` choice from factory templates.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure`, `asNumericObservable`, and `asTimeObservable` methods were removed).

Version 2.0.0
*************

- Changed the signature of `advance` and `rewind` in the `Reference.Time` interface.

- Dependencies update.
