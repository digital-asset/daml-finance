.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Settlement
##########

:ref:`Settlement <settlement>` refers to the execution of holding transfers originating from
a financial transaction.

For instance, an example FX spot transaction involves the transfer of a
EUR-denominated holding from Alice to Bob in exchange for a
USD-denominated holding.

The library provides facilities to execute these transfers atomically
(i.e., within the same Daml transaction) in the package ``Daml.Finance.Interface.Settlement``.
An implementation is provided in ``Daml.Finance.Settlement``.

Step
****

The FX example transaction above contains two steps:

#. transfer EUR from Alice to Bob
#. transfer USD from Bob to Alice

They are represented using one ``Step`` each.
The step defines who is the sender, who is the receiver and what should be transferred (instrument and amount).

Instruction
***********

A ``Step`` is not sufficient to do a transfer. We also need to know exactly which holding should be used and to which account it should be transferred.
This is specified in an ``Instruction`` (one for each ``Step``).
The ``Instruction`` allows the sender to specify which holding to transfer by exercising the ``Allocate`` choice.
The receiver can then specify which account should be used by exercising the ``Approve`` choice.

Batch
*****

We could execute the transfer of the two instructions above individually, but that would cause
a problem if one instruction fails and the other one succeeds. Instead, we want to execute them
simultaneously in one atomic transaction. We can do that by using a ``Batch`` contract.

These settlement concepts are also explained with example code in the :doc:`Settlement tutorial <../tutorials/getting-started/settlement>`.
