.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Extending Daml Finance
######################

Daml Finance is designed to be extended whenever the provided implementations do not satisfy the
requirements at hand. In principle, all interfaces in the :ref:`interface layer <interface-layer>`
can be implemented with custom implementations. Specific extension points we expect and encourage
users to customize are explained below.

Note that for all of the listed extension points, we are happy to receive external contributions to
be included in the library.

Custom Holding Implementations
******************************

Daml Finance provides default holding implementations for the following holding standards:

1. fungible only (i.e., the `Fungible` holding standard)
2. transferable only (i.e., `Transferable` holding standard)
3. both transferable and fungible (i.e., the `TransferableFungible` holding standard)
4. neither fungible nor transferable (i.e., the `BaseHolding` holding standard)

The transferability of transferable holdings can be flexibly controlled through the
:ref:`controllers <type-daml-finance-interface-account-v4-account-controllers-59817>`
property on an :ref:`Account <module-daml-finance-account-v4-account-5834>`.
Some use cases, however, might require additional functionality on holding contracts:

- *Restricted transferability*: a custom implementation of the
  :ref:`Transferable interface <module-daml-finance-interface-holding-v4-transferable-93054>`
  can enforce additional conditions (e.g. the presence of some contract) required to transfer a
  holding.
- *Fixed divisibility*: a custom implementation of the
  :ref:`Fungible interface <module-daml-finance-interface-holding-v4-fungible-55495>` can enforce
  specific requirements regarding the divisibility of a holding.
- *Additional information*: a custom implementation of a holding can provide additional information,
  for example, the timestamp of when the holding was obtained. This can be used to implement
  features that depend on the time a particular asset has been held (e.g. holding fees, interest,
  etc.).

Note that any custom holding implementation will still allow you to leverage other parts of the
library (e.g. lifecycling or settlement) as those are implemented against the respective interfaces.
You will need to provide an implementation of the
:ref:`Holding Factory <module-daml-finance-interface-holding-v4-factory-49942>` interface for
your implementation to be usable throughout the library.

Custom Account Implementations
******************************

The default account implementation in Daml Finance allows you to define authorization requirements
for incoming and outgoing transfers through the
:ref:`controllers <type-daml-finance-interface-account-v4-account-controllers-59817>` property.
For some cases, however, a custom account implementation may be warranted:

- Restricted credit and debit: a custom implementation of the ``Credit`` and / or
  ``Debit`` choices on the
  :ref:`Account interface <module-daml-finance-interface-account-v4-account-30007>` can place
  additional restrictions on those actions that can depend, for example, on the presence of a
  separate know-your-customer (KYC) contract.
- Additional information: a custom account implementation can serve to represent different concepts
  of accounts. For example, a shelf in a vault for gold bars or a specific location within a
  warehouse can be represented by providing additional information on an account implementation.
- Account Freezing: an account can optionally implement
  the :ref:`Lockable <module-daml-finance-interface-util-v3-lockable-20339>` interface, allowing it to
  be frozen, i.e., temporarily disabling credits and debits.

Custom Instrument Implementations
*********************************

Daml Finance provides default implementations for a wide range of financial instruments. However, we
anticipate that specific requirements will lead to the adaptation of existing, or the creation of
entirely new instrument types. The following are typical examples of when a custom instrument
implementation is required:

- Additional information: a custom instrument implementation might, for example, build upon the
  :ref:`Equity interface <module-daml-finance-interface-instrument-equity-v0-instrument-90217>` to
  provide additional information pertinent to private equity (like share class, or liquidation
  preference).
- New instrument types: if Daml Finance does not provide an implementation for a given instrument
  type, a custom implementation can be provided to fill that gap. The implementation can either
  leverage the :doc:`Contingent Claims <../instruments/generic/contingent-claims>` framework, as
  described in
  :doc:`this tutorial <../tutorials/advanced-topics/instrument-modeling/contingent-claims-instrument>`,
  or be implemented through standard interfaces, as seen in the implementation of the
  :ref:`Equity instrument <module-daml-finance-instrument-equity-v0-instrument-40246>`.

Custom Lifecycle Implementations
********************************

Daml Finance provides a default set of lifecycle rules that can be used to evolve instruments.
Examples are the implementation of
:ref:`Distributions <module-daml-finance-lifecycle-v4-rule-distribution-2662>`,
:ref:`Replacements <module-daml-finance-lifecycle-v4-rule-replacement-25183>`, or the
:ref:`time-based evolution <module-daml-finance-interface-lifecycle-v4-event-time-69757>`
of contingent-claims based instruments. There are many more lifecycle events and rules
that can be implemented using the provided interfaces. Typically, implementations of the
:ref:`Event <module-daml-finance-interface-lifecycle-v4-event-91777>` and
:ref:`Rule <module-daml-finance-interface-lifecycle-v4-rule-lifecycle-8270>` interface are required to
handle new lifecycle events. Examples of events where a library extension might be warranted
include:

- Credit events on bonds: our bond implementations don't provide an implementation for handling
  default events, as these are highly case-specific. A custom lifecycle event and rule
  implementation can provide the logic to handle the treatment of bond positions in case of default.
- Special corporate actions: a distribution that is either restricted to, or dependent on certain
  conditions can be implemented through a custom lifecycle implementation.
- Custom evolution logic: a non-fungible token following a specific evolution logic (i.e., it can be
  evolved under certain circumstances) can be implemented using custom lifecycle events and rules.

Custom Settlement Implementations
*********************************

Daml Finance aims to provide a flexible and powerful mechanism to orchestrate asset settlement.
There are cases, however, where a custom implementation might be required:

- Off-ledger integrations: specific information might be required to facilitate handling of
  settlement instructions in off-ledger rails. This could include, for example, information required
  to create SWIFT messages.
- Cross-ledger settlement: mechanisms like Hashed Timelock Contracts or custodial-bridged settlement
  might require a custom implementation of the settlement choices.
