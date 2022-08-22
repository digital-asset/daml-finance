.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-types-84427:

Module Daml.Finance.Interface.Instrument.Generic.Types
==============================================

Data Types
----------

.. _type-daml-finance-interface-instrument-generic-types-c-63687:

**type** `C <type-daml-finance-interface-instrument-generic-types-c-63687_>`_
  \= Claim `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ `Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-67765_>`_ `Observable <type-daml-finance-interface-instrument-generic-types-observable-46520_>`_

  The specialized claim type

.. _type-daml-finance-interface-instrument-generic-types-deliverable-67765:

**type** `Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-67765_>`_
  \= :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`

  Type used to reference assets in the claim tree\.

.. _type-daml-finance-interface-instrument-generic-types-observable-46520:

**type** `Observable <type-daml-finance-interface-instrument-generic-types-observable-46520_>`_
  \= `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  Type used to reference observables in the claim tree\.

.. _type-daml-finance-interface-instrument-generic-types-pending-91971:

**data** `Pending <type-daml-finance-interface-instrument-generic-types-pending-91971_>`_

  Type used to record pending payments

  .. _constr-daml-finance-interface-instrument-generic-types-pending-25908:

  `Pending <constr-daml-finance-interface-instrument-generic-types-pending-25908_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - t
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         -
       * - tag
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - instrument
         - `Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-67765_>`_
         -
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         -

.. _type-daml-finance-interface-instrument-generic-types-taggedclaim-29758:

**data** `TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758_>`_

  A claim and a textual tag

  .. _constr-daml-finance-interface-instrument-generic-types-taggedclaim-43249:

  `TaggedClaim <constr-daml-finance-interface-instrument-generic-types-taggedclaim-43249_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - claim
         - `C <type-daml-finance-interface-instrument-generic-types-c-63687_>`_
         -
       * - tag
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758_>`_

  **instance** HasMethod :ref:`HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-95955>` \"getClaims\" (`Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758_>`_\])

Functions
---------

.. _function-daml-finance-interface-instrument-generic-types-taggedclaim-32586:

`taggedClaim <function-daml-finance-interface-instrument-generic-types-taggedclaim-32586_>`_
  \: `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> `C <type-daml-finance-interface-instrument-generic-types-c-63687_>`_ \-\> `TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758_>`_

  Tagged claim constructor
