.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Instrument Extensions
#####################

Tokens
======

- ``Daml.Finance.Interface.Instrument.Base``

    This package contains the *interface* for a basic instrument. It contains the following module:

    - :ref:`Instrument <module-daml-finance-interface-instrument-base-instrument-57320>`:
      Base interface for an instrument, which does not define any lifecycling logic.

- ``Daml.Finance.Instrument.Token``

    This package contains the *implementation* of a basic instrument. It contains the following
    module:

    - :ref:`Instrument <module-daml-finance-instrument-token-instrument-10682>`:
      Standard implementation of an instrument, which does not define any lifecycling logic

    Check out the :doc:`Transfer tutorial <../tutorials/getting-started/transfer>` for an example on
    how to create a base instrument and use it for a transfer.

Bonds
=====

- ``Daml.Finance.Interface.Instrument.Bond``

    This package contains the *interface* for different bond types, defined in the following
    instrument modules:

    - :ref:`FixedRate <module-daml-finance-interface-instrument-bond-fixedrate-factory-42532>`:
      Interface that allows implementing templates to create fixed rate bonds
    - :ref:`FloatingRate <module-daml-finance-interface-instrument-bond-floatingrate-factory-53843>`:
      Interface that allows implementing templates to create floating rate bonds
    - :ref:`InflationLinked <module-daml-finance-interface-instrument-bond-inflationlinked-factory-57553>`:
      Interface that allows implementing templates to create inflation linked bonds
    - :ref:`ZeroCoupon <module-daml-finance-interface-instrument-bond-zerocoupon-factory-70433>`:
      Interface that allows implementing templates to create zero coupon bonds

- ``Daml.Finance.Instrument.Bond``

    This package contains the *implementation* of different bond types, defined in the following
    instrument modules:

    - :ref:`FixedRate <module-daml-finance-instrument-bond-fixedrate-instrument-67993>`:
      This template models a fixed rate bond that pays a fixed coupon rate at the end of every
      coupon period
    - :ref:`FloatingRate <module-daml-finance-instrument-bond-floatingrate-instrument-98586>`:
      This template models a floating rate bond that pays a floating coupon rate at the end of every
      coupon period
    - :ref:`InflationLinked <module-daml-finance-instrument-bond-inflationlinked-instrument-30250>`:
      This template models an inflation linked bond that pays an inflation-adjusted coupon rate at
      the end of every coupon period
    - :ref:`ZeroCoupon <module-daml-finance-instrument-bond-zerocoupon-instrument-52804>`:
      This template models a zero coupon bond that does not pay any coupons, only the redemption
      amount at maturity

    Check out the tutorial on
    :doc:`How to use the Bond extension package <../tutorials/instrument-modeling/bond-extension>`
    for a description how to use the bond extension in practice. There is also the tutorial
    :doc:`How to implement a Contingent Claims-based instrument <../tutorials/instrument-modeling/contingent-claims-instrument>`,
    which describes how the claims are defined and how the lifecycle interface is implemented for
    bonds.

Swaps
=====

- ``Daml.Finance.Interface.Instrument.Swap``

    This package contains the *interface* for different types of swaps. It contains the following
    instrument modules:

    - :ref:`Asset <module-daml-finance-interface-instrument-swap-asset-instrument-37258>`:
      Interface that allows implementing templates to create asset swaps
    - :ref:`CreditDefaultSwap <module-daml-finance-interface-instrument-swap-creditdefault-instrument-27480>`:
      Interface that allows implementing templates to create credit default swaps
    - :ref:`CurrencySwap <module-daml-finance-interface-instrument-swap-currency-instrument-11782>`:
      Interface that allows implementing templates to create currency swaps
    - :ref:`ForeignExchange <module-daml-finance-interface-instrument-swap-foreignexchange-instrument-90743>`:
      Interface that allows implementing templates to create foreign exchange (FX) swaps
    - :ref:`InterestRate <module-daml-finance-interface-instrument-swap-interestrate-instrument-49463>`:
      Interface that allows implementing templates to create interest rate swaps
    - :ref:`Fpml <module-daml-finance-interface-instrument-swap-fpml-instrument-38654>`:
      Interface that allows implementing templates to create FpML swaps

- ``Daml.Finance.Instrument.Swap``

    This package contains the *implementation* of different types of swaps. It contains the
    following instrument modules:

    - :ref:`Asset <module-daml-finance-instrument-swap-asset-instrument-28127>`:
      This template models an asset swap that pays a fixed rate vs the performance of an asset at
      the end of every payment period
    - :ref:`CreditDefaultSwap <module-daml-finance-instrument-swap-creditdefault-instrument-88725>`:
      This template models a credit default swap that pays a fixed rate vs *1-recoveryRate* (in case
      of a credit default event)
    - :ref:`CurrencySwap <module-daml-finance-instrument-swap-currency-instrument-67721>`:
      This template models a currency swap that pays a fixed vs fixed rate (in different currencies)
      at the end of every payment period
    - :ref:`ForeignExchange <module-daml-finance-instrument-swap-foreignexchange-instrument-43394>`:
      This template models a foreign exchange (FX) swap with two legs: an initial FX transaction and
      a final FX transaction
    - :ref:`InterestRate <module-daml-finance-instrument-swap-interestrate-instrument-86260>`:
      This template models an interest rate swap that pays a fixed vs floating interest rate at the
      end of every payment period
    - :ref:`Fpml <module-daml-finance-instrument-swap-fpml-instrument-17241>`:
      This template models interest rate swaps using the
      `FpML swap schema <https://www.fpml.org/spec/fpml-5-11-3-lcwd-1/html/confirmation/schemaDocumentation/schemas/fpml-ird-5-11_xsd/complexTypes/Swap.html>`_

Equity
======

- ``Daml.Finance.Interface.Instrument.Equity``

    This package contains the *interface* for equities. It has the following modules:

    - :ref:`Factory <module-daml-finance-interface-instrument-equity-factory-97140>`:
      Interface that allows implementing templates to create equity instruments
    - :ref:`Instrument <module-daml-finance-interface-instrument-equity-instrument-13224>`:
      Interface for a generic equity instrument

- ``Daml.Finance.Instrument.Equity``

    This package contains the *implementation* for equities. It has the following modules:

    - :ref:`Factory <module-daml-finance-instrument-equity-factory-96899>`:
      Factory template for instrument creation
    - :ref:`Instrument <module-daml-finance-instrument-equity-instrument-69265>`:
      Instrument representing a common stock

    For a detailed explanation of the equity extension, check out the
    ``src/test/daml/Daml/Finance/Instrument/Equity/Test`` folder. It demonstrates how to originate
    an equity instrument, how to create and lifecycle a cash dividend, and how to handle corporate
    actions like mergers and stock splits.

Generic
=======

- ``Daml.Finance.Interface.Instrument.Generic``

    This package contains the *interface* and types required for generic instruments using
    ``Contingent Claims``, including lifecycling logic. It contains the following modules:

    - :ref:`Election <module-daml-finance-interface-instrument-generic-election-94835>`:
      Interface implemented by templates that represents a (claim-based) election
    - :ref:`Factory <module-daml-finance-interface-instrument-generic-factory-11761>`:
      Interface that allows implementing templates to create generic instruments

- ``Daml.Finance.Instrument.Generic``

    This package contains the *implementation* and types required for generic instruments, including
    lifecycling logic. It contains the following modules:

    - :ref:`Election <module-daml-finance-instrument-generic-election-56972>`:
      Implementation of Election (e.g. the exercise of an option) and ElectionFactory (to delegate
      the right to create Elections)
    - :ref:`Factory <module-daml-finance-instrument-generic-factory-42712>`:
      Factory template for generic instrument creation
    - :ref:`Instrument <module-daml-finance-instrument-generic-instrument-67364>`:
      An instrument representing a generic payoff, modelled using ``Contingent Claims``
    - :ref:`Util <module-daml-finance-instrument-generic-util-13331>`:
      Utility functions related to creating Contingent Claims for bonds/swaps including lifecycling
      logic

    The tutorial :doc:`How to use the Generic extension to model generic instruments <../tutorials/instrument-modeling/generic-extension>`
    describes how a payoff is defined using ``Contingent Claims`` in practice.
