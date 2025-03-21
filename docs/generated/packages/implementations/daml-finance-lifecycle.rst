.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Lifecycle.V4
#########################

This package contains the *implementation* of lifecycle related processes. It contains the following
modules:

- :ref:`Effect <module-daml-finance-lifecycle-v4-effect-31424>`:
  A contract encoding the *consequences of a lifecycle event* for one unit of the target
  instrument
- :ref:`Election <module-daml-finance-lifecycle-v4-election-2732>`:
  Implementation of elections (e.g. the exercise of an option) for claim based instruments
- :ref:`ElectionEffect <module-daml-finance-lifecycle-v4-electioneffect-83755>`:
  A contract encoding the *consequences of an election* for one unit of the target instrument
- :ref:`Rule.Claim <module-daml-finance-lifecycle-v4-rule-claim-11721>`:
  Rule contract that allows an actor to process/claim effects, returning settlement instructions
- :ref:`Rule.Distribution <module-daml-finance-lifecycle-v4-rule-distribution-2662>`:
  Rule contract that defines the distribution of units of an instrument for each unit of a
  target instrument (e.g. share or cash dividends)
- :ref:`Rule.Replacement <module-daml-finance-lifecycle-v4-rule-replacement-25183>`:
  Rule contract that defines the replacement of units of an instrument with a basket of other
  instruments (e.g. stock merger)
- :ref:`Rule.Util <module-daml-finance-lifecycle-v4-rule-util-70932>`:
  Utility functions to net, split and merge pending payments
- :ref:`Event.Distribution <module-daml-finance-lifecycle-v4-event-distribution-38493>`:
  Event contract for the distribution of units of an instrument for each unit of a target
  instrument (e.g. share or cash dividends)
- :ref:`Event.Replacement <module-daml-finance-lifecycle-v4-event-replacement-94706>`:
  Event contract for the replacement of units of an instrument with a basket of other
  instruments (e.g. stock merger)

Check out the :doc:`Lifecycling tutorial <../../tutorials/getting-started/lifecycling>` for a
description on how lifecycling works in practice, including how to
:ref:`Claim <module-daml-finance-interface-lifecycle-v4-rule-claim-89954>` an
:ref:`Effect <module-daml-finance-interface-lifecycle-v4-effect-48507>`.
There is also the tutorial
:doc:`How to implement a Contingent Claims-based instrument <../../tutorials/advanced-topics/instrument-modeling/contingent-claims-instrument>`,
which describes how to create an
:ref:`Effect <module-daml-finance-interface-lifecycle-v4-effect-48507>`.
For a description of ``Distribution`` and ``Replacement``, check out the
`Instrument/Equity/Test <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Equity/Test>`_
folder. It demonstrates how to create and lifecycle a cash dividend, and how to handle corporate
actions like mergers and stock splits.

Changelog
*********
