.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Instrument.Equity
########################################

This package contains the *interface* definitions for equity instruments. It contains the following
modules:

- :ref:`Instrument <module-daml-finance-interface-instrument-equity-instrument-13224>`:
  Instrument interface for equities. It supports lifecycling events through the
  ``DeclareDividend``, ``DeclareReplacement`` and ``DeclareStockSplit`` choices.
- :ref:`Factory <module-daml-finance-interface-instrument-equity-factory-97140>`:
  Factory interface to instantiate equities
