.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-types-37112:

Module Daml.Finance.Interface.Instrument.Generic.Types
======================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-generic-types-c-8090:

**type** `C <type-daml-finance-interface-instrument-generic-types-c-8090_>`_
  \= Claim `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ `Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-56164_>`_ `Observable <type-daml-finance-interface-instrument-generic-types-observable-24391_>`_
  
  The specialized claim type

.. _type-daml-finance-interface-instrument-generic-types-deliverable-56164:

**type** `Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-56164_>`_
  \= :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-58546>`
  
  Type used to reference assets in the claim tree\.

.. _type-daml-finance-interface-instrument-generic-types-observable-24391:

**type** `Observable <type-daml-finance-interface-instrument-generic-types-observable-24391_>`_
  \= `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
  
  Type used to reference observables in the claim tree\.

.. _type-daml-finance-interface-instrument-generic-types-pending-79018:

**data** `Pending <type-daml-finance-interface-instrument-generic-types-pending-79018_>`_

  Type used to record pending payments
  
  .. _constr-daml-finance-interface-instrument-generic-types-pending-41353:
  
  `Pending <constr-daml-finance-interface-instrument-generic-types-pending-41353_>`_
  
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
         - `Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-56164_>`_
         - 
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - 

.. _type-daml-finance-interface-instrument-generic-types-taggedclaim-22591:

**data** `TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-22591_>`_

  A claim and a textual tag
  
  .. _constr-daml-finance-interface-instrument-generic-types-taggedclaim-37164:
  
  `TaggedClaim <constr-daml-finance-interface-instrument-generic-types-taggedclaim-37164_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - claim
         - `C <type-daml-finance-interface-instrument-generic-types-c-8090_>`_
         - 
       * - tag
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - 
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-22591_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-22591_>`_
  
  **instance** HasMethod :ref:`HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-75942>` \"getClaims\" (`Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-22591_>`_\])

Functions
---------

.. _function-daml-finance-interface-instrument-generic-types-taggedclaim-74363:

`taggedClaim <function-daml-finance-interface-instrument-generic-types-taggedclaim-74363_>`_
  \: `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> `C <type-daml-finance-interface-instrument-generic-types-c-8090_>`_ \-\> `TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-22591_>`_
  
  Tagged claim constructor
