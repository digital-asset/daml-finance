.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Experimental Extensions (early access)
######################################

- Bonds
    - ``Daml.Finance.Exp.Interface.Instrument.Bond`` This package contains the *interface* for different bond types: :ref:`FixedRate <module-daml-finance-interface-instrument-bond-fixedrate-31328>`, :ref:`FloatingRate <module-daml-finance-interface-instrument-bond-floatingrate-5967>`, :ref:`InflationLinked <module-daml-finance-interface-instrument-bond-inflationlinked-64713>` and :ref:`ZeroCoupon <module-daml-finance-interface-instrument-bond-zerocoupon-20445>`.
    - ``Daml.Finance.Exp.Instrument.Bond`` This package contains the *implementation* of different bonds types: :ref:`FixedRate <module-daml-finance-instrument-bond-fixedrate-44039>`, :ref:`FloatingRate <module-daml-finance-instrument-bond-floatingrate-31782>`, :ref:`InflationLinked <module-daml-finance-instrument-bond-inflationlinked-38254>` and :ref:`ZeroCoupon <module-daml-finance-instrument-bond-zerocoupon-72656>`. It also contains some :ref:`Util <module-daml-finance-instrument-bond-util-70458>` functions, which are used to create claims and perform lifecycling for bonds.

    Check out the tutorial on :doc:`How to use the Bond extension package <../../tutorial/instrument-modelling/bond-extension>` for a description how to use the bond extension in practice.
    There is also the tutorial :doc:`How to implement a Contingent Claims-based instrument <../../tutorial/instrument-modelling/contingent-claims-instrument>`, which describes how the claims are defined and how the lifecycle interface is implemented for bonds.

- Equities
    - ``Daml.Finance.Exp.Interface.Instrument.Equity`` This package contains the *interface* for equities. The :ref:`Factory <module-daml-finance-interface-instrument-equity-factory-97140>` interface allows implementing templates to create instruments. The :ref:`Instrument <module-daml-finance-interface-instrument-equity-instrument-13224>` contains an interface for a generic equity instrument.
    - ``Daml.Finance.Exp.Instrument.Equity`` This package contains the *implementation* of equities. The :ref:`Factory <module-daml-finance-instrument-equity-factory-96899>` template is used to create equity instruments. The :ref:`Instrument <module-daml-finance-instrument-equity-instrument-69265>` template represents a common stock.

    For a detailed explanation of the equity extension, check out the ``src/test/daml/Daml/Finance/Instrument/Equity/Test`` folder. It demonstrates how to originate an equity instrument,
    how to create and lifecycle a cash dividend, and how to handle corporate actions like mergers and stock splits.