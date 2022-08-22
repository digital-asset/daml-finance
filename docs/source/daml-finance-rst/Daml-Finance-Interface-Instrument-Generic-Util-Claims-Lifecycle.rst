.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-util-claims-lifecycle-94563:

Module Daml.Finance.Interface.Instrument.Generic.Util.Claims.Lifecycle
==============================================================

Functions
---------

.. _function-daml-finance-interface-instrument-generic-util-claims-lifecycle-timeevent-21709:

`timeEvent <function-daml-finance-interface-instrument-generic-util-claims-lifecycle-timeevent-21709_>`_
  \: `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> Event

  Constructor for a time event\.

.. _function-daml-finance-interface-instrument-generic-util-claims-lifecycle-electionevent-22867:

`electionEvent <function-daml-finance-interface-instrument-generic-util-claims-lifecycle-electionevent-22867_>`_
  \: `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`C <type-daml-finance-interface-instrument-generic-types-c-63687>` \-\> Event

  Constructor for an election event\.

.. _function-daml-finance-interface-instrument-generic-util-claims-lifecycle-lifecycleclaims-74529:

`lifecycleClaims <function-daml-finance-interface-instrument-generic-util-claims-lifecycle-lifecycleclaims-74529_>`_
  \: \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-observable-i-63746>`\] \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\] \-\> \[Event\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (\[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\], \[:ref:`Pending <type-daml-finance-interface-instrument-generic-types-pending-91971>`\])

  Lifecycle a set of claims at specified events\.

.. _function-daml-finance-interface-instrument-generic-util-claims-lifecycle-lifecycle-15332:

`lifecycle <function-daml-finance-interface-instrument-generic-util-claims-lifecycle-lifecycle-15332_>`_
  \: \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-observable-i-63746>`\] \-\> :ref:`I <type-daml-finance-interface-instrument-generic-hasclaims-i-90893>` \-\> \[Event\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (\[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\], \[:ref:`Pending <type-daml-finance-interface-instrument-generic-types-pending-91971>`\])

  Lifecycle a claim instrument at specified events\.

.. _function-daml-finance-interface-instrument-generic-util-claims-lifecycle-splitpending-45812:

`splitPending <function-daml-finance-interface-instrument-generic-util-claims-lifecycle-splitpending-45812_>`_
  \: \[:ref:`Pending <type-daml-finance-interface-instrument-generic-types-pending-91971>`\] \-\> (\[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-31714>`\], \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-31714>`\])

  Map pending settlements into corresponding instrument quantities and split them into consumed and produced\.
  Pending items with an amount of ``0.0`` are discarded\.
