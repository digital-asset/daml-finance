.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-claims-v4-claim-38573:

Daml.Finance.Interface.Claims.V4.Claim
======================================

Interfaces
----------

.. _type-daml-finance-interface-claims-v4-claim-claim-91182:

**interface** `Claim <type-daml-finance-interface-claims-v4-claim-claim-91182_>`_

  Interface implemented by templates that can be represented as Contingent Claims\.

  **viewtype** `V <type-daml-finance-interface-claims-v4-claim-v-42696_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-claims-v4-claim-getclaims-15445:

    **Choice** `GetClaims <type-daml-finance-interface-claims-v4-claim-getclaims-15445_>`_

    Retrieves the list of claims representing the instrument\. This might involve fetching
    reference data, such as calendars, on which the actor must have visibility\.

    Controller\: actor

    Returns\: \[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\]

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actor
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the claims\.

  + .. _type-daml-finance-interface-claims-v4-claim-getview-95007:

    **Choice** `GetView <type-daml-finance-interface-claims-v4-claim-getview-95007_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-claims-v4-claim-view-18692_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method getClaims \:** `GetClaims <type-daml-finance-interface-claims-v4-claim-getclaims-15445_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\]

    The list of claims representing the instrument\.

Data Types
----------

.. _type-daml-finance-interface-claims-v4-claim-i-57743:

**type** `I <type-daml-finance-interface-claims-v4-claim-i-57743_>`_
  \= `Claim <type-daml-finance-interface-claims-v4-claim-claim-91182_>`_

  Type synonym for ``Claim``\.

.. _type-daml-finance-interface-claims-v4-claim-v-42696:

**type** `V <type-daml-finance-interface-claims-v4-claim-v-42696_>`_
  \= `View <type-daml-finance-interface-claims-v4-claim-view-18692_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Claim <type-daml-finance-interface-claims-v4-claim-claim-91182_>`_ `V <type-daml-finance-interface-claims-v4-claim-v-42696_>`_

.. _type-daml-finance-interface-claims-v4-claim-view-18692:

**data** `View <type-daml-finance-interface-claims-v4-claim-view-18692_>`_

  View for ``Claim``\.

  .. _constr-daml-finance-interface-claims-v4-claim-view-62295:

  `View <constr-daml-finance-interface-claims-v4-claim-view-62295_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - acquisitionTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The claim's acquisition time\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-claims-v4-claim-view-18692_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-claims-v4-claim-view-18692_>`_

Functions
---------

.. _function-daml-finance-interface-claims-v4-claim-getclaims-87153:

`getClaims <function-daml-finance-interface-claims-v4-claim-getclaims-87153_>`_
  \: `Claim <type-daml-finance-interface-claims-v4-claim-claim-91182_>`_ \-\> `GetClaims <type-daml-finance-interface-claims-v4-claim-getclaims-15445_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\]

.. _function-daml-finance-interface-claims-v4-claim-getclaim-68311:

`getClaim <function-daml-finance-interface-claims-v4-claim-getclaim-68311_>`_
  \: `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Claim <type-daml-finance-interface-claims-v4-claim-claim-91182_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ :ref:`C <type-daml-finance-interface-claims-v4-types-c-76802>`

  Retrieves the single claim representing the template\. An error is thrown if there are zero or
  more than one claims\.

.. _function-daml-finance-interface-claims-v4-claim-getacquisitiontime-25831:

`getAcquisitionTime <function-daml-finance-interface-claims-v4-claim-getacquisitiontime-25831_>`_
  \: `Claim <type-daml-finance-interface-claims-v4-claim-claim-91182_>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  Retrieves the claim's acquisition time\.
