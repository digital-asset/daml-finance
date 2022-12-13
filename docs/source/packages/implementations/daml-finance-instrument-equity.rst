.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Instrument.Equity
##############################

This package contains the *implementation* of equity instruments, defined in the following modules:

- :ref:`Instrument <module-daml-finance-instrument-equity-instrument-69265>`:
  Instrument implementation for equities
- :ref:`Factory <module-daml-finance-instrument-equity-factory-96899>`:
  Factory implementation to instantiate equities

Check out the tutorial on
:doc:`How to use the Equity extension package <../../tutorials/instrument-modeling/equity-extension>`.

For a detailed explanation of the equity extension, check out the
``src/test/daml/Daml/Finance/Instrument/Equity/Test`` folder. It demonstrates how to originate
an equity instrument, how to create and lifecycle a cash dividend, and how to handle corporate
actions like mergers and stock splits.
