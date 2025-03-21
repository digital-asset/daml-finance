.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-claims-v4-types-7840:

Daml.Finance.Interface.Claims.V4.Types
======================================

Data Types
----------

.. _type-daml-finance-interface-claims-v4-types-c-76802:

**type** `C <type-daml-finance-interface-claims-v4-types-c-76802_>`_
  \= :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ `Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084_>`_ `Observable <type-daml-finance-interface-claims-v4-types-observable-11919_>`_

  The specialized claim type\.

.. _type-daml-finance-interface-claims-v4-types-deliverable-51084:

**type** `Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084_>`_
  \= :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  Type used to reference assets in the claim tree\.

.. _type-daml-finance-interface-claims-v4-types-observable-11919:

**type** `Observable <type-daml-finance-interface-claims-v4-types-observable-11919_>`_
  \= `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  Type used to reference observables in the claim tree\.

.. _type-daml-finance-interface-claims-v4-types-pending-22818:

**data** `Pending <type-daml-finance-interface-claims-v4-types-pending-22818_>`_

  Type used to record pending payments\.

  .. _constr-daml-finance-interface-claims-v4-types-pending-2747:

  `Pending <constr-daml-finance-interface-claims-v4-types-pending-2747_>`_

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
         - `Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084_>`_
         -
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Pending <type-daml-finance-interface-claims-v4-types-pending-22818_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Pending <type-daml-finance-interface-claims-v4-types-pending-22818_>`_

.. _type-daml-finance-interface-claims-v4-types-taggedclaim-85831:

**data** `TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831_>`_

  A claim and a textual tag\.

  .. _constr-daml-finance-interface-claims-v4-types-taggedclaim-83974:

  `TaggedClaim <constr-daml-finance-interface-claims-v4-types-taggedclaim-83974_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - claim
         - `C <type-daml-finance-interface-claims-v4-types-c-76802_>`_
         -
       * - tag
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831_>`_

  **instance** HasMethod :ref:`Claim <type-daml-finance-interface-claims-v4-claim-claim-91182>` \"getClaims\" (:ref:`GetClaims <type-daml-finance-interface-claims-v4-claim-getclaims-15445>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831_>`_\])
