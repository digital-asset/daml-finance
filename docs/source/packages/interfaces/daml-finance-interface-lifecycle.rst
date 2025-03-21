.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Lifecycle.V4
###################################

This package contains the *interface* for lifecycle related processes. It contains the following
modules:

- :ref:`Effect <module-daml-finance-interface-lifecycle-v4-effect-48507>`:
  Interface for contracts exposing effects of lifecycling processes, e.g., the payment resulting
  from a bond coupon
- :ref:`Election <module-daml-finance-interface-lifecycle-v4-election-15483>`:
  Interface to allow for elections to be made on claim based instruments
- :ref:`Election.Factory <module-daml-finance-interface-lifecycle-v4-election-factory-21763>`:
  Factory interface to instantiate elections on claim based instruments
- :ref:`Event <module-daml-finance-interface-lifecycle-v4-event-91777>`:
  Interface for a lifecycle event. An event is any contract that triggers the processing of a
  lifecycle rule. Events can be, e.g., dividend announcements or simply the passing of time.
- :ref:`Event.Distribution <module-daml-finance-interface-lifecycle-v4-event-distribution-56030>`:
  Event interface for the distribution of units of an instrument for each unit of a target
  instrument (e\.g\. share or cash dividends)
- :ref:`Event.Replacement <module-daml-finance-interface-lifecycle-v4-event-replacement-41051>`:
  Event interface for the replacement of units of an instrument with a basket of other
  instruments (e\.g\. stock merger)
- :ref:`Event.Time <module-daml-finance-interface-lifecycle-v4-event-time-69757>`:
  Event interface for events that signal the passing of (business) time
- :ref:`Rule.Lifecycle <module-daml-finance-interface-lifecycle-v4-rule-lifecycle-8270>`:
  Interface implemented by rules that lifecycle and evolve instruments
- :ref:`Rule.Claim <module-daml-finance-interface-lifecycle-v4-rule-claim-89954>`:
  Interface for contracts that allow holders to claim an ``Effect`` and generate settlement
  instructions
- :ref:`Observable.NumericObservable <module-daml-finance-interface-lifecycle-v4-observable-numericobservable-50817>`:
  Interface to observe time-dependent numerical values (e.g. a stock price or an interest rate
  fixing)
- :ref:`Observable.TimeObservable <module-daml-finance-interface-lifecycle-v4-observable-timeobservable-64296>`:
  Interface implemented by templates exposing time information

The :doc:`Lifecycling <../../concepts/lifecycling>` page contains an overview of the lifecycle
process and explains the relationship between events, lifecycle rules and effects. Check out the
:doc:`Lifecycling tutorial <../../tutorials/getting-started/lifecycling>` for a description on how
lifecycling works in practice. There is also the tutorial
:doc:`How to implement a Contingent Claims-based instrument <../../tutorials/advanced-topics/instrument-modeling/contingent-claims-instrument>`,
which describes how claims are defined, how to use a ``NumericObservable``, and how the
``Lifecycle`` interface is implemented for bonds.

The following diagram shows the incoming and outgoing dependencies for this package:

.. image:: ../../images/daml_finance_interface_lifecycle.png
   :alt: A diagram showing the incoming and outgoing dependencies of the package.

Changelog
*********
