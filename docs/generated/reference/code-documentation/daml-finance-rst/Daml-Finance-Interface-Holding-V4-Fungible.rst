.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-holding-v4-fungible-55495:

Daml.Finance.Interface.Holding.V4.Fungible
==========================================

Interfaces
----------

.. _type-daml-finance-interface-holding-v4-fungible-fungible-31071:

**interface** `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_

  Interface for a fungible holding\.

  **viewtype** `V <type-daml-finance-interface-holding-v4-fungible-v-97914_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-holding-v4-fungible-getview-5733:

    **Choice** `GetView <type-daml-finance-interface-holding-v4-fungible-getview-5733_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-holding-v4-fungible-view-93398_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party fetching the view\.

  + .. _type-daml-finance-interface-holding-v4-fungible-merge-76684:

    **Choice** `Merge <type-daml-finance-interface-holding-v4-fungible-merge-76684_>`_

    Merge multiple fungible contracts into a single fungible contract\.

    Controller\: (DA\.Internal\.Record\.getField @\"modifiers\" (view this))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - fungibleCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_\]
         - The fungible contracts to merge which will get consumed\.

  + .. _type-daml-finance-interface-holding-v4-fungible-split-16580:

    **Choice** `Split <type-daml-finance-interface-holding-v4-fungible-split-16580_>`_

    Split a fungible contract into multiple contracts by amount\.

    Controller\: (DA\.Internal\.Record\.getField @\"modifiers\" (view this))

    Returns\: `SplitResult <type-daml-finance-interface-holding-v4-fungible-splitresult-97497_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - amounts
         - \[`Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_\]
         - The quantities to split the fungible asset by, creating a new contract per amount\.

  + **Method merge \:** `Merge <type-daml-finance-interface-holding-v4-fungible-merge-76684_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_)

    Implementation of the ``Merge`` choice\.

  + **Method split \:** `Split <type-daml-finance-interface-holding-v4-fungible-split-16580_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `SplitResult <type-daml-finance-interface-holding-v4-fungible-splitresult-97497_>`_

    Implementation of the ``Split`` choice\.

Data Types
----------

.. _type-daml-finance-interface-holding-v4-fungible-i-95581:

**type** `I <type-daml-finance-interface-holding-v4-fungible-i-95581_>`_
  \= `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_

  Type synonym for ``Fungible``\.

.. _type-daml-finance-interface-holding-v4-fungible-splitresult-97497:

**data** `SplitResult <type-daml-finance-interface-holding-v4-fungible-splitresult-97497_>`_

  Result of a call to ``Split``\.

  .. _constr-daml-finance-interface-holding-v4-fungible-splitresult-96424:

  `SplitResult <constr-daml-finance-interface-holding-v4-fungible-splitresult-96424_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - splitCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_\]
         - The contract ids for the split holdings\.
       * - rest
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_)
         - Contract id for the holding on the remaining amount\. It is ``None`` when the split is exact\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `SplitResult <type-daml-finance-interface-holding-v4-fungible-splitresult-97497_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `SplitResult <type-daml-finance-interface-holding-v4-fungible-splitresult-97497_>`_

  **instance** HasMethod `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_ \"split\" (`Split <type-daml-finance-interface-holding-v4-fungible-split-16580_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `SplitResult <type-daml-finance-interface-holding-v4-fungible-splitresult-97497_>`_)

.. _type-daml-finance-interface-holding-v4-fungible-v-97914:

**type** `V <type-daml-finance-interface-holding-v4-fungible-v-97914_>`_
  \= `View <type-daml-finance-interface-holding-v4-fungible-view-93398_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_ `V <type-daml-finance-interface-holding-v4-fungible-v-97914_>`_

.. _type-daml-finance-interface-holding-v4-fungible-view-93398:

**data** `View <type-daml-finance-interface-holding-v4-fungible-view-93398_>`_

  View for ``Fungible``\.

  .. _constr-daml-finance-interface-holding-v4-fungible-view-32885:

  `View <constr-daml-finance-interface-holding-v4-fungible-view-32885_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - modifiers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties which have the authorization to modify a fungible asset\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-holding-v4-fungible-view-93398_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-holding-v4-fungible-view-93398_>`_

Functions
---------

.. _function-daml-finance-interface-holding-v4-fungible-split-85728:

`split <function-daml-finance-interface-holding-v4-fungible-split-85728_>`_
  \: `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_ \-\> `Split <type-daml-finance-interface-holding-v4-fungible-split-16580_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `SplitResult <type-daml-finance-interface-holding-v4-fungible-splitresult-97497_>`_

.. _function-daml-finance-interface-holding-v4-fungible-merge-52600:

`merge <function-daml-finance-interface-holding-v4-fungible-merge-52600_>`_
  \: `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_ \-\> `Merge <type-daml-finance-interface-holding-v4-fungible-merge-76684_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Fungible <type-daml-finance-interface-holding-v4-fungible-fungible-31071_>`_)
