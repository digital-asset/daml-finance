.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Leverage Contingent Claims in Custom Instrument Implementations
###############################################################

They :doc:`Payoff Modeling tutorial <../../payoff-modeling/intro>` introduces the
:doc:`Contingent Claims <../../../instruments/generic/contingent-claims>` modeling framework
in the context of the :doc:`Generic Instrument <../../../instruments/generic>`.
In this chapter, we will see how the library can instead be used to lifecycle custom instrument
implementations. The :doc:`Bond <../../../instruments/bond>` and
:doc:`Swap <../../../instruments/swap>` instruments, for example, leverage Contingent Claims
behind the scenes to calculate pending coupon payments.

Let us explore in detail how the
:ref:`fixed rate bond instrument <module-daml-finance-instrument-bond-v3-fixedrate-instrument-89221>`
is implemented in Daml Finance. The goal is for you to learn how to implement and lifecycle your own
instrument template, should you need an instrument type that is not already implemented in the
library.

To follow the code snippets used in this tutorial in Daml Studio, you can clone the
`Daml Finance repository <https://github.com/digital-asset/daml-finance>`_ and take a look at how
the
:ref:`Bond Instrument template <module-daml-finance-instrument-bond-v3-fixedrate-instrument-89221>`
is implemented. In order to see how lifecycling is performed, you can run the script in the
`Instrument/Bond/Test/FixedRate.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml>`_
file.

Template Definition
===================

We start by defining a new template for the instrument. Here are the fields used for the fixed
rate instrument:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/V3/FixedRate/Instrument.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_TEMPLATE_BEGIN
  :end-before: -- FIXED_RATE_BOND_TEMPLATE_END

These template variables describe the economic terms of a fixed rate bond.

The ``Claims`` Interface
========================

We now need to map the template variables to a
:doc:`Contingent Claims <../../../instruments/generic/contingent-claims>` tree, the internal
representation we wish to use for lifecycling.
Note that the Contingent Claims tree is not a part of the template above, instead it will be created
dynamically upon request.

In order do that, we implement the
:ref:`Claims interface <module-daml-finance-interface-claims-v4-claim-38573>`.
This interface provides access to a generic mechanism to process coupon payments and redemptions.
It will work in a similar way for the majority of instrument types, regardless of their specific
economic terms.

Here is a high level implementation of the
:ref:`Claims interface <module-daml-finance-interface-claims-v4-claim-38573>`:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/V3/FixedRate/Instrument.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_CLAIMS_BEGIN
  :end-before: -- FIXED_RATE_BOND_CLAIMS_END

The ``getClaims`` function is where we define the payoff of the instrument.

* First, we calculate the coupon payment dates by rolling out a periodic coupon schedule.

* The payment dates are then used to build claim sub-trees for the coupon payments.

* A claim sub-tree for the final redemption is also created.

* Finally, the coupon and redemption sub-trees are joined. Together, they yield the desired economic
  terms for the bond.

How to define the redemption claim
**********************************

The redemption claim depends on the currency and the maturity date of the bond.

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Claims/V3/Util/Builders.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_REDEMPTION_CLAIM_BEGIN
  :end-before: -- FIXED_RATE_BOND_REDEMPTION_CLAIM_END

Keywords like
:ref:`when <function-contingentclaims-core-v3-claim-when-40851>`,
:ref:`scale <function-contingentclaims-core-v3-claim-scale-39304>`,
:ref:`one <function-contingentclaims-core-v3-claim-one-90688>` and
:ref:`give <function-contingentclaims-core-v3-claim-give-33092>` should be familiar from the
:doc:`Payoff Modeling tutorial <../../payoff-modeling/intro>`.
:ref:`TimeGte <constr-contingentclaims-core-v3-internal-claim-timegte-43192>` is just a synonym for
:ref:`at <function-contingentclaims-core-v3-claim-at-41106>`.

The ``ownerReceives`` flag is used to indicate whether the owner of a holding on the bond is meant
to receive the redemption payment. When this is set to ``false``, the holding custodian will be
entitled to the payment.

How to define the coupon claims
*******************************

The coupon claims are a bit more complicated to define.
We need to take a schedule of adjusted coupon dates and the day count convention into account.

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Claims/V3/Util/Builders.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_COUPON_CLAIMS_BEGIN
  :end-before: -- FIXED_RATE_BOND_COUPON_CLAIMS_END

For each coupon period, we calculate the adjusted end date and the actual coupon amount. We then
create each coupon claim in a way similar to the redemption claim above.

Evolving the Instrument over time
=================================

The bond instrument gives the holder the right to receive future coupons and the redemption amount.
At issuance, all coupons are due. However, after the first coupon is paid, the holder of the
instrument is no longer entitled to receive it again.
The ``lastEventTimestamp`` field in our template is used to keep track of the latest executed coupon
payment.

Evolution of the instrument over time (and calculation of the corresponding lifecycle effects) can
be performed using the :ref:`Lifecycle.Rule <module-daml-finance-claims-v3-lifecycle-rule-196>`
template provided in the
:doc:`Daml.Finance.Claims.V3 <../../../packages/implementations/daml-finance-claims>` package. This
rule is very generic and can be used for all instruments that implement the
:ref:`Claims interface <module-daml-finance-interface-claims-v4-claim-38573>`.

Let us break its implementation apart to describe what happens in more detail:

* First, we retrieve the claim tree corresponding to the initial state of the instrument. We do so
  by fetching the ``Claims`` interface we defined for the template.

  .. literalinclude:: ../../../src/main/daml/Daml/Finance/Claims/V3/Lifecycle/Rule.daml
    :language: daml
    :start-after: -- BOND_PROCESS_CLOCK_UPDATE_INITAL_CLAIMS_BEGIN
    :end-before: -- BOND_PROCESS_CLOCK_UPDATE_INITAL_CLAIMS_END

* By using the ``lastEventTimestamp`` (in our case: the last time a coupon was paid), we can now
  "fast forward" the claim tree to the current instrument state.

  .. literalinclude:: ../../../src/main/daml/Daml/Finance/Claims/V3/Lifecycle/Rule.daml
    :language: daml
    :start-after: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_FASTFORWARD_BEGIN
    :end-before: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_FASTFORWARD_END

* Finally, we lifecycle the current instrument state to calculate pending
  `effects <#lifecycling-effect>`__. If there are such effects (for example when a coupon payment is
  due), we create a :ref:`Lifecycle Effect <module-daml-finance-lifecycle-v4-effect-31424>` for it,
  which can then be claimed and settled.

  .. literalinclude:: ../../../src/main/daml/Daml/Finance/Claims/V3/Lifecycle/Rule.daml
    :language: daml
    :start-after: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_BEGIN
    :end-before: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_END

The ``Dynamic.Instrument`` Interface
====================================

In the ``tryCreateNewInstrument`` part above, we create a new version of the instrument containing
the updated ``lastEventTimestamp`` (and also including all previous events up until now). This is
done by exercising the
:ref:`CreateNewVersion <type-daml-finance-interface-claims-v4-dynamic-instrument-createnewversion-58950>`
choice of the
:ref:`Dynamic.Instrument interface <module-daml-finance-interface-claims-v4-claim-38573>`:

  .. literalinclude:: ../../../src/main/daml/Daml/Finance/Claims/V3/Lifecycle/Rule.daml
    :language: daml
    :start-after: -- CREATE_NEW_DYNAMIC_INSTRUMENT_VERSION_BEGIN
    :end-before: -- CREATE_NEW_DYNAMIC_INSTRUMENT_VERSION_END

This ensures that the next time the instrument is lifecycled, the current coupon is no longer
included. This also works for other types of events, for example a barrier hit on a derivative
instrument: if such an event is ever lifecycled it will persist on (a new version of) the
instrument, ensuring that it will not be forgotten when the instrument is lifecycled in the future.

Including market observables
============================

.. ML not sure we need this section, as it is already covered in the lifecycling tutorial

In our fixed rate bond example above, the coupon amount is pre-determined at the inception of the
instrument. In contrast, a floating rate coupon is defined by the value of a reference rate during
the lifetime of the bond. Since we do not know this value when the instrument is created, we need to
define the coupon based on a future observation of the reference rate.

In the instrument definition, we need an identifier for the reference rate:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/V3/FloatingRate/Instrument.daml
  :language: daml
  :start-after: -- FLOATING_RATE_BOND_TEMPLATE_UNTIL_REFRATE_BEGIN
  :end-before: -- FLOATING_RATE_BOND_TEMPLATE_UNTIL_REFRATE_END

When we create the claims for the coupon payments, we can then use
:ref:`ObserveAt <constr-contingentclaims-core-v3-observation-observeat-93788>` to refer to the value of
the reference rate:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Claims/V3/Util/Builders.daml
  :language: daml
  :start-after: -- FLOATING_RATE_BOND_COUPON_CLAIMS_BEGIN
  :end-before: -- FLOATING_RATE_BOND_COUPON_CLAIMS_END

In this example, the observable is a reference interest rate. Other instrument types can require
other types of observables, such as an FX rate or a stock price.

Different ways to create and store the Contingent Claims tree
=============================================================

To summarize what we have seen so far, there are two different ways of using
:doc:`Contingent Claims <../../../instruments/generic/contingent-claims>` in a Daml Finance
instrument.

When using the :doc:`Generic Instrument <../../../instruments/generic>`, we create the claim tree
at instrument inception and store this representation explicitly on the ledger. After a lifecycle
event, for example a coupon payment, a new version of the instrument (with a different claim tree)
supersedes the previous version.

In contrast, the approach used in this tutorial only stores the key economic terms of the bond
on the ledger. The claim tree is not stored on-ledger, but it is created "on-the-fly" when needed
(for example, when lifecycling).

Which approach is preferred?
****************************

The latter approach has the advantage that the claim tree can adapt to changes in reference
data. A change to e.g. a holiday calendar would automatically impact the claim tree the next time it
is dynamically created. This is not the case for the first approach, where the tree is static.

Also, if the economic terms of the instrument result in a very large claim tree, it could be
desirable not to store it on the ledger for performance reasons.

Finally, the "dynamic" approach allows for the terms of the template to be very descriptive to
anyone familiar with the payoff at hand.

On the other hand, if you need to quickly create a one-off instrument, the on-ledger approach allows
you to create the claims directly from a script, without first having to define a dedicated
template.

Also, if you need to explicitly access Contingent Claims representations of older versions of
the instrument on the ledger, for example for auditing reasons, that would be achieved out of the
box with the first approach.
