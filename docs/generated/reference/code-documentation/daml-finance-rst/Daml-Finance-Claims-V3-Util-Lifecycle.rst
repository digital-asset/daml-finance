.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-claims-v3-util-lifecycle-43238:

Daml.Finance.Claims.V3.Util.Lifecycle
=====================================

Functions
---------

.. _function-daml-finance-claims-v3-util-lifecycle-timeevent-64726:

`timeEvent <function-daml-finance-claims-v3-util-lifecycle-timeevent-64726_>`_
  \: `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> EventData

  Constructor for a time event\.

.. _function-daml-finance-claims-v3-util-lifecycle-electionevent-54352:

`electionEvent <function-daml-finance-claims-v3-util-lifecycle-electionevent-54352_>`_
  \: `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> EventData

  Constructor for an election event\.

.. _function-daml-finance-claims-v3-util-lifecycle-lifecycleclaims-45610:

`lifecycleClaims <function-daml-finance-claims-v3-util-lifecycle-lifecycleclaims-45610_>`_
  \: \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-i-61855>`\] \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ \-\> \[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\] \-\> \[EventData\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (\[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\], \[:ref:`Pending <type-daml-finance-interface-claims-v4-types-pending-22818>`\])

  Lifecycle a set of claims at specified events\.

.. _function-daml-finance-claims-v3-util-lifecycle-netontag-25394:

`netOnTag <function-daml-finance-claims-v3-util-lifecycle-netontag-25394_>`_
  \: \[:ref:`Pending <type-daml-finance-interface-claims-v4-types-pending-22818>`\] \-\> \[:ref:`Pending <type-daml-finance-interface-claims-v4-types-pending-22818>`\]

  Net pending payments on the same instrument, which also have the same tag\.

.. _function-daml-finance-claims-v3-util-lifecycle-lifecycle-71991:

`lifecycle <function-daml-finance-claims-v3-util-lifecycle-lifecycle-71991_>`_
  \: `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-i-61855>`\] \-\> :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` \-\> \[EventData\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (\[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\], \[:ref:`Pending <type-daml-finance-interface-claims-v4-types-pending-22818>`\])

  Lifecycle a claim instrument at specified events\.

.. _function-daml-finance-claims-v3-util-lifecycle-splitpending-46301:

`splitPending <function-daml-finance-claims-v3-util-lifecycle-splitpending-46301_>`_
  \: \[:ref:`Pending <type-daml-finance-interface-claims-v4-types-pending-22818>`\] \-\> (\[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\], \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\])

  Map pending settlements into corresponding instrument quantities and split them into consumed
  and produced\. Pending items with an amount of ``0.0`` are discarded\.
