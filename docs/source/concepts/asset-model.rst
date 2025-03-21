.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Asset Model
###########

The library’s asset model is the set of contracts that describe the financial rights and obligations
that exist between parties. It is composed of instruments, holdings, and accounts.

Instrument
**********

An instrument contract describes the economic terms (rights and obligations) of one unit of a
financial contract.

It can be as simple as an ISIN code referencing some real-world (off-ledger) security, or it can
encode specific on-ledger lifecycling logic.

Additionally, it specifies the instrument's holding standard or properties, which is described in
more detail in the :ref:`Holding Standards <holding-standards>` section.

Signatories
===========

Every instrument must have an ``issuer`` party and a ``depository`` party, which are both
signatories of the contract.

The terminology is borrowed from the real world. For example, an issuer of a stock instrument
deposits the paper certificate at a depository and gets the corresponding amount credited in
book-entry form.

On the ledger, the ``depository`` acts as a trusted party that prevents the ``issuer`` from
potentially acting maliciously.

Keys and Versioning
===================

Instruments are keyed by an
:ref:`InstrumentKey <constr-daml-finance-interface-types-common-v3-types-instrumentkey-49116>`,
which comprises:

- the instrument ``issuer``
- the instrument ``depository``
- a textual ``id``
- a textual ``version``
- the instrument's ``holdingStandard``

The version is used to keep track of the linear evolution of an instrument. For example, once a
dividend on a share is paid, the version is used to identify the cum-dividend and the ex-dividend
share.

Interfaces
==========

Instrument interfaces are defined in the ``Daml.Finance.Interface.Instrument.*`` packages.

All instruments must implement the base interface, defined in
:ref:`Daml.Finance.Interface.Instrument.Base.V4 <module-daml-finance-interface-instrument-base-v4-instrument-47185>`.

Implementations
===============

A base implementation is provided in
:ref:`Daml.Finance.Instrument.Token.V4 <module-daml-finance-instrument-token-v4-instrument-53415>`.

This template does not define any lifecycling logic and is suitable to model contracts that are
likely to stay stable, such as currency instruments.

The extension packages provide additional business-specific implementations, such as an
:ref:`Equity <module-daml-finance-instrument-equity-v0-instrument-40246>`
instrument (where the issuer can pay dividends) or a
:ref:`Bond <module-daml-finance-instrument-bond-v3-fixedrate-instrument-89221>`
instrument (which includes coupon payments).

The expectation is that customers define their own instruments suiting the use-case they are
modeling.

.. _holding-asset-model:

Holding
*******

A holding contract represents the ownership of a certain amount of an instrument by an owner at a
custodian.

Whereas an instrument defines *what* a party holds (the rights and obligations), a holding defines
*how much* (i.e., the amount) of an instrument and *against which party* (i.e., the custodian) the
instrument is being held.

It is important to understand that the economic terms of an asset (the instrument) are separated
from the representation of an asset holding. This allows centralized management of instruments (e.g.
lifecycling) and the reuse of instruments and associated logic across different entities (e.g.
custodians). It also avoids the data redundancy of replicating instrument data and logic on every
holding contract.

.. _signatories-1:

Signatories
===========

Every holding must have an ``owner`` party and a ``custodian`` party,
which are usually both signatories of the contract.

The terminology is again borrowed from the real world: our cash or shares are usually deposited at a
custodian and we have (at least in principle) the right to claim them back from the custodian at any
given time.

.. _holding-standards:

Holding Standards
=================

A holding implementation can have specific properties such as being :ref:`fungible <fungibility>`
and/or :ref:`transferable <transferability>`.

When, for instance, a holding is transferable, the ownership can be transferred to a different party
at the same custodian.

These properties are exposed by letting a holding template implement the corresponding interfaces
(:ref:`Fungible <module-daml-finance-interface-holding-v4-fungible-55495>` and
:ref:`Transferable <module-daml-finance-interface-holding-v4-transferable-93054>`,
respectively).

The library distinguishes four types of holdings, referred to as :ref:`Holding Standard
<type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`\s, namely:

1. `Fungible`: Holdings that are fungible only.
2. `Transferable`: Holdings that are transferable only.
3. `TransferableFungible`: Holdings that are both transferable and fungible.
4. `BaseHolding`: Holdings that are neither transferable nor fungible.

Interfaces
==========

Holding interfaces are defined in the ``Daml.Finance.Interface.Holding.V4`` package. It consists of
the interfaces :ref:`holding <module-daml-finance-interface-holding-v4-holding-20535>`,
:ref:`transferable <module-daml-finance-interface-holding-v4-transferable-93054>`, and
:ref:`fungible <module-daml-finance-interface-holding-v4-fungible-55495>`.

.. _implementations-1:

Implementations
===============

``Daml.Finance.Holding.V4`` provides implementations for each holding standard:

- :ref:`fungible only <module-daml-finance-holding-v4-fungible-60188>`
- :ref:`transferable only <module-daml-finance-holding-v4-transferable-38649>`
- :ref:`both transferable and fungible <module-daml-finance-holding-v4-transferablefungible-66907>`
- :ref:`neither transferable nor fungible <module-daml-finance-holding-v4-baseholding-28133>`

Account
*******

Account contracts are used as proof of a relationship between a ``custodian`` and an ``owner``.

An ``owner`` must have an account contract with a ``custodian`` before a holding contract can be
created between the two parties.

This is similar to how, in the real world, you need to open a bank account before you can use the
bank’s services.

The account contract also controls which parties are authorized to transfer holdings in and out of
the account. To be more precise, the
:ref:`controllers <type-daml-finance-interface-account-v4-account-controllers-59817>`
field of the account contains:

- ``outgoing``: a set of parties authorizing outgoing transfers
- ``incoming``: a set of parties authorizing incoming transfers

This allows for modeling various controllers of transfers between Alice's and Bob's accounts. For
example:

- owners-controlled: If the ``owner`` is the sole member the ``outgoing`` and ``incoming``
  controllers for the accounts, a transfer of a holding from Alice's account to Bob's account needs
  to be authorized jointly by Alice and Bob.
- owner-only-controlled: If, instead, there are no ``incoming`` controllers of Bob's account,
  it is enough that Alice authorizes the transfer alone.
- custodian-controlled: If, as often is the case, the ``custodian`` needs to control what is being
  transferred, we can instead let the ``custodian`` be the sole member of ``outgoing`` and
  ``incoming`` controllers of the accounts.

Accounts also serve to prevent holding transfers to unvetted third parties: a holding of Alice can
only be transferred to Bob if Bob has an account at the same Bank (and has therefore been vetted by
the Bank).

.. _signatories-2:

Signatories
===========

An account is co-signed by the account ``owner`` and the ``custodian``.

Keys
====

Accounts are keyed by an
:ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`, which
comprises:

- the account ``owner``
- the account ``custodian``
- a textual ``id``

.. _implementations-2:

Interfaces
==========

The account interface is defined in the
:ref:`Daml.Finance.Interface.Account.V4 <module-daml-finance-interface-account-v4-account-30007>`
package.

Implementations
===============

A base account implementation is provided in
:ref:`Daml.Finance.Account.V4 <module-daml-finance-account-v4-account-5834>`.

The account can be created with arbitrary
:ref:`controllers <type-daml-finance-interface-account-v4-account-controllers-59817>`
(for incoming and outgoing transfers). Our examples typically let accounts be owners-controlled,
i.e., both the current owner and the new owner must authorize transfers.

The account also implements
the :ref:`Lockable <module-daml-finance-interface-util-v3-lockable-20339>` interface, enabling the
freezing of an account, thus disabling credits and debits.

Example setups
**************

We can now look at a few examples of how real-world rights and obligations can be modeled using the
Daml Finance asset model.

Currency
========

We start by modeling a standard cash bank account. There are three parties involved: a Central Bank,
a Commercial Bank, and a Retail Client.

The Central Bank defines the economic terms of the currency asset and is generally a highly trusted
entity, therefore it acts as ``issuer`` as well as ``depository`` of the corresponding instrument.

We can use the :ref:`Token <module-daml-finance-instrument-token-v4-instrument-53415>`
instrument implementation for a currency asset, as we do not need any lifecycling logic.

The Retail Client has an
:ref:`Account <module-daml-finance-interface-account-v4-account-30007>` at the Commercial Bank, with
the former acting as ``owner`` and the latter as ``custodian``.

Finally, the Retail Client is ``owner`` of a
:ref:`transferable and fungible holding <module-daml-finance-holding-v4-transferablefungible-66907>` at
the Commercial Bank (i.e., the ``custodian`` in the contract). The holding references the currency
instrument and the account.

.. image:: ../images/asset_model_currency.png
   :alt: Currency asset setup.

In this scenario, we can see how:

- the instrument defines what is held
- the holding defines where the rights and obligations lie, as well as the corresponding amount

Equity
======

We now model units of shares held by an investor. There are three parties involved: an Issuing
Entity, a Securities Depository, and an Investor.

The Issuing Entity acts as ``issuer`` of the :ref:`Equity Instrument
<module-daml-finance-instrument-equity-v0-instrument-40246>`. The Securities Depository acts
as ``depository`` of the instrument, thus preventing the Issuing Entity from single-handedly
modifying details of the instrument (such as the share's nominal value).

The Institutional Investor holds units of shares against the Securities Depository, through
corresponding Account and Holding contracts.

.. image:: ../images/asset_model_stock.png
   :alt: Equity asset setup.

It is worth noting that the ``issuer`` of the Equity Instrument has the right to perform certain
Corporate Actions, such as declaring dividends. This topic is covered in the
:doc:`lifecycling section <lifecycling>`.

.. _otc-swap-asset-model:

OTC Swap
========

Finally, we model an OTC (over-the-counter) fixed vs. floating interest rate swap agreement between
two parties, namely Party A and Party B. We can use the :ref:`Interest Rate Swap
<module-daml-finance-instrument-swap-v0-interestrate-instrument-37965>` instrument template
for this purpose.

In this case, all contracts are agreed and co-signed by both parties. In the instrument contract,
it does not really matter whether Party A is the ``issuer`` and Party B the ``depository``, or the
other way around. However, the role matters in the Holding contract, as it defines the direction of
the trade, i.e., which party receives the fixed leg and which party receives the floating one.

.. image:: ../images/asset_model_otc.png
   :alt: OTC Swap asset setup.
