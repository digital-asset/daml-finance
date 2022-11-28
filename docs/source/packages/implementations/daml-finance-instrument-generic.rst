.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Instrument.Generic
###############################

This package contains the *implementation* of generic, contingent-claims-based instruments, defined
in the following modules:

- :ref:`Instrument <module-daml-finance-instrument-generic-instrument-67364>`:
  Instrument implementation for generic instruments
- :ref:`Factory <module-daml-finance-instrument-generic-factory-42712>`:
  Factory implementation to instantiate generic instruments
- :ref:`Election <module-daml-finance-instrument-generic-election-56972>`:
  Implementation of elections (e.g. the exercise of an option) for generic instruments

The tutorial :doc:`How To Model and Lifecycle Generic Instruments <../../tutorials/instrument-modeling/generic-extension>`
describes how a payoff is defined and lifecycled using ``Contingent Claims`` in practice.
