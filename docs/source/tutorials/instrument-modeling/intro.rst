.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Instrument Modeling
###################

.. toctree::
   :hidden:

   bond-extension
   bond-lifecycling
   swap-extension
   generic-extension
   contingent-claims-instrument
   contingent-claims-on-ledger-vs-on-the-fly

This section explains different topics related to modeling instruments using Daml Finance. Each
tutorial combines a step by step description of different workflows with supporting code.

The following tutorials are available:

* The :doc:`Bond Extension <bond-extension>` tutorial introduces the different types of bonds
  supported in Daml Finance out of the box.
* The :doc:`Bond Lifecycling <bond-lifecycling>` tutorial demonstrates how to lifecycle a simple
  bond and process coupon payments.
* The :doc:`Swap Extension <swap-extension>` tutorial describes how to use the different types of
  swaps in Daml Finance.
* The :doc:`Generic Extension <generic-extension>` tutorial shows you how to define your own generic
  instrument.
* The :doc:`Contingent Claims Instrument <contingent-claims-instrument>` tutorial describes how to
  create a new instrument type (similar to the bond instruments that you saw in the Bond Extension
  above).
* The :doc:`On ledger vs on-the-fly <contingent-claims-on-ledger-vs-on-the-fly>` tutorial helps you
  to decide whether to explicitly store the claims tree on the ledger or generate it on-the-fly.
