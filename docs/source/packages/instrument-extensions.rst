.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Instrument Extensions
#####################

Tokens
======

- ``Daml.Finance.Interface.Instrument.Base``

    This package contains the *interface* for a basic instrument. It contains the following modules:

    - :ref:`Factory <module-daml-finance-interface-instrument-base-factory-89800>`: Interface that allows implementing templates to create instruments.
    - :ref:`Instrument <module-daml-finance-interface-instrument-base-instrument-57320>`: Base interface for an instrument. This interface does not define any lifecycling logic.

- ``Daml.Finance.Instrument.Base``

    This package contains the *implementation* of a basic instrument. It contains the following module:

    - :ref:`Instrument <module-daml-finance-instrument-base-instrument-53549>`: Base implementation of an Instrument which does not define any lifecycling logic.

    Check out the :doc:`Transfer tutorial <../../tutorials/getting-started/transfer>` for an example on how to create a base instrument and use it for a transfer.

Bonds
=====

- ``Daml.Finance.Interface.Instrument.Bond``

    This package contains the *interface* for different bond types, defined in the following modules:

    - :ref:`FixedRate <module-daml-finance-interface-instrument-bond-fixedrate-31328>`: Interface that allows implementing templates to create fixed rate bonds.
    - :ref:`FloatingRate <module-daml-finance-interface-instrument-bond-floatingrate-5967>`: Interface that allows implementing templates to create floating rate bonds.
    - :ref:`InflationLinked <module-daml-finance-interface-instrument-bond-inflationlinked-64713>`: Interface that allows implementing templates to create inflation linked bonds.
    - :ref:`ZeroCoupon <module-daml-finance-interface-instrument-bond-zerocoupon-20445>`: Interface that allows implementing templates to create zero coupon bonds.

- ``Daml.Finance.Instrument.Bond``

    This package contains the *implementation* of different bond types, defined in the following modules:

    - :ref:`FixedRate <module-daml-finance-instrument-bond-fixedrate-44039>`: This template models a fixed rate bond. It pays a fixed coupon rate at the end of every coupon period.
    - :ref:`FloatingRate <module-daml-finance-instrument-bond-floatingrate-31782>`: This template models a floating rate bond. It pays a floating coupon rate at the end of every coupon period.
    - :ref:`InflationLinked <module-daml-finance-instrument-bond-inflationlinked-38254>`: This template models an inflation linked bond. It pays an inflation adjusted coupon at the end of every coupon period.
    - :ref:`ZeroCoupon <module-daml-finance-instrument-bond-zerocoupon-72656>`: This template models a zero coupon bond. It does not pay any coupons, only the redemption amount at maturity.
    - :ref:`Util <module-daml-finance-instrument-bond-util-70458>`: Utility functions related to creating Contingent Claims for coupons / redemption and bond lifecycling logic.

    Check out the tutorial on :doc:`How to use the Bond extension package <../../tutorials/instrument-modelling/bond-extension>` for a description how to use the bond extension in practice.
    There is also the tutorial :doc:`How to implement a Contingent Claims-based instrument <../../tutorials/instrument-modelling/contingent-claims-instrument>`, which describes how the claims are defined and how the lifecycle interface is implemented for bonds.

Equity
======

- ``Daml.Finance.Interface.Instrument.Equity``

    This package contains the *interface* for equities. It has the following modules:

    - :ref:`Factory <module-daml-finance-interface-instrument-equity-factory-97140>`: Interface that allows implementing templates to create equity instruments.
    - :ref:`Instrument <module-daml-finance-interface-instrument-equity-instrument-13224>`: Interface for a generic equity instrument.

- ``Daml.Finance.Instrument.Equity``

    This package contains the *implementation* for equities. It has the following modules:

    - :ref:`Factory <module-daml-finance-instrument-equity-factory-96899>`: Factory template for instrument creation.
    - :ref:`Instrument <module-daml-finance-instrument-equity-instrument-69265>`: Instrument representing a common stock.

    For a detailed explanation of the equity extension, check out the ``src/test/daml/Daml/Finance/Instrument/Equity/Test`` folder. It demonstrates how to originate an equity instrument,
    how to create and lifecycle a cash dividend, and how to handle corporate actions like mergers and stock splits.

Generic
=======

- ``Daml.Finance.Interface.Instrument.Generic``

    This package contains the *interface* and types required for generic instruments using ``Contingent Claims``, including lifecycling logic. It contains the following modules:

    - :ref:`Util.Claims.Lifecycle <module-daml-finance-interface-instrument-generic-util-claims-lifecycle-66560>`: Defines different types of events and how to lifecycle them.
    - :ref:`Util.Claims <module-daml-finance-interface-instrument-generic-util-claims-70604>`: Contains utility functions for claims, e.g. checking content of a claim and converting claim time.
    - :ref:`Election <module-daml-finance-interface-instrument-generic-election-94835>`: Interface implemented by templates that represents a (claim-based) election.
    - :ref:`Factory <module-daml-finance-interface-instrument-generic-factory-11761>`: Interface that allows implementing templates to create generic instruments.
    - :ref:`HasClaims <module-daml-finance-interface-instrument-generic-hasclaims-47920>`: Interface implemented by templates that can be represented as a set of contingent claims.
    - :ref:`Types <module-daml-finance-interface-instrument-generic-types-37112>`: Types related to claims and what is require to represent claims (e.g. Deliverable and Observable.)

- ``Daml.Finance.Instrument.Generic``

    This package contains the *implementation* and types required for generic instruments, including lifecycling logic. It contains the following modules:

    - :ref:`Election <module-daml-finance-instrument-generic-election-56972>`: Implementation of Election (e.g. the exercise of an option) and ElectionFactory (to delegate the right to create Elections).
    - :ref:`Factory <module-daml-finance-instrument-generic-factory-42712>`: Factory template for generic instrument creation.
    - :ref:`Instrument <module-daml-finance-instrument-generic-instrument-67364>`: An instrument representing a generic payoff, modelled using ``Contingent Claims``.

    The tutorial :doc:`How to use the Derivative extension to model generic instruments <../../tutorials/instrument-modelling/derivative-extension>` describes how a payoff is defined using ``Contingent Claims`` in practice.
