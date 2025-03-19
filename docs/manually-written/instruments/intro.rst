.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Instruments
###########

This section describes which instruments are included out of the box in Daml Finance. Each
instrument package contains a list of supported instruments. The instrument extension pages explain
what each instrument does and how to set it up.

Bonds
*****

The following instruments are included in the :doc:`Bond Instrument package <bond>`:

- Fixed rate bonds
- Floating rate bonds
- Callable bonds
- Inflation linked bonds
- Zero coupon bonds

Equites
*******

The following instruments are included in the :doc:`Equity Instrument package <equity>`:

- Equities (can also be used to model ETFs)

Options
*******

The following instruments are included in the :doc:`Option Instrument package <option>`:

- Cash-settled European options
- Physically-settled European options
- Barrier options
- Dividend options

Structured Products
*******************

The following instruments are included in the
:doc:`Structured Products Instrument package <structured-product>`:

- Barrier reverse convertible

Swaps
*****

The following instruments are included in the :doc:`Swap Instrument package <swap>`:

- Interest rate swaps
- Currency swaps
- Foreign exchange swaps
- Credit default swaps
- Asset swaps
- FpML swaps (supports the above swap types using the FpML schema)

Other Instruments
*****************

In addition to the above instruments, which model specific payoffs, the library provides

- a :doc:`Token Instrument <token>`, whose terms are defined by a simple textual label

- a :doc:`Generic Instrument <generic>`, which provides a flexible framework to
  structure user-defined payoffs and lifecycle them on the ledger