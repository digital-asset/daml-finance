.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Glossary
########

This page defines some of the terminology used in the Daml Finance library.

We strive to use descriptive names and stay as close as possible to the traditional financial
meaning of familiar terms.

.. _account:

Account
-------

An account contract is a relationship between two parties: a custodian (or account provider) and an
owner.

An account is referenced by `holdings <#holding>`__ and it is used to control who is entitled to
instruct and receive holding transfers.

.. _instrument:

Instrument
----------

An instrument describes the economic terms (rights and obligations) of one unit of a financial
contract.

An instrument is referenced by `holdings <#holding>`__. It can be as simple as an ISIN code
referencing real-world (off-ledger) security, or it can encode specific on-ledger lifecycling logic.

.. _holding:

Holding
-------

A holding contract represents the ownership of a certain amount of an `Instrument <#instrument>`__
by an owner at a custodian.

Custodian
---------

The custodian is the party registering ownership for a given `Instrument <#instrument>`__.

Owner
-----

The owner is the party holding legal (or beneficial) ownership of an `Instrument <#instrument>`__.

Depository
----------

The depository is the party responsible for safekeeping the legal form of an
`Instrument <#instrument>`__ (e.g. paper certificates).

Issuer
------

The issuer is the party issuing new units of an `Instrument <#instrument>`__.

.. _fungibility:

Fungibility
-----------

Fungibility refers to the ability of an `Instrument <#instrument>`__ to be interchanged with other
individual instruments of the same type.

Only a fungible instrument can be held for an amount other than ``1.0``.

.. _transferability:

Transferability
---------------

Transferability refers to the ability to transfer ownership of units of an
`Instrument <#instrument>`__ to a new owner at the same custodian.

.. _locking:

Locking
-------

Locking is a mechanism that adds a third-party authorization requirement to any interaction with a
`Holding <#holding>`__ (archive, transfer, split, merge, etc.).

It is used to ensure that holdings committed to a certain workflow are not consumed by other
workflows.

Crediting / Debiting
--------------------

Crediting is the process of creating new `Holdings <#holding>`__ for a given instrument and
debiting, conversely, is removing existing ones.

Disclosure
----------

Disclosure is the ability to reveal a contract to a third party by adding them as an observer.

.. _settlement:

Settlement
----------

Settlement is the (possibly simultaneous) execution of ownership transfers according to predefined
instructions.

Many financial transactions are traditionally settled a few days after execution.

.. _lifecycling:

Lifecycling
-----------

Lifecycling refers to the evolution of `Instruments <#instrument>`__ over their lifetime.

Lifecycling can deal with intrinsic events, like contractual cash flows, and/or extrinsic events
like corporate actions or elections.
