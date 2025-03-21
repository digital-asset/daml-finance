.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-holding-v4-transferable-93054:

Daml.Finance.Interface.Holding.V4.Transferable
==============================================

Interfaces
----------

.. _type-daml-finance-interface-holding-v4-transferable-transferable-83505:

**interface** `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_

  An interface respresenting a contract where ownership can be transferred to other parties\.

  **viewtype** `View <type-daml-finance-interface-holding-v4-transferable-view-4631_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-holding-v4-transferable-getview-81598:

    **Choice** `GetView <type-daml-finance-interface-holding-v4-transferable-getview-81598_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-holding-v4-transferable-view-4631_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-holding-v4-transferable-transfer-3593:

    **Choice** `Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593_>`_

    Transfer a contract to a new owner\.

    Controller\: actors

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties authorizing the transfer\.
       * - newOwnerAccount
         - :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`
         - The new owner's account\.

  + **Method transfer \:** `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-transferable-i-68214_>`_ \-\> `Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_)

    Implementation of the ``Transfer`` choice\.

Data Types
----------

.. _type-daml-finance-interface-holding-v4-transferable-i-68214:

**type** `I <type-daml-finance-interface-holding-v4-transferable-i-68214_>`_
  \= `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_

  Type synonym for ``Transferable``\.

  **instance** HasMethod `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_ \"transfer\" (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-transferable-i-68214_>`_ \-\> `Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_))

.. _type-daml-finance-interface-holding-v4-transferable-v-641:

**type** `V <type-daml-finance-interface-holding-v4-transferable-v-641_>`_
  \= `View <type-daml-finance-interface-holding-v4-transferable-view-4631_>`_

  Type synonym for ``View``\.

.. _type-daml-finance-interface-holding-v4-transferable-view-4631:

**data** `View <type-daml-finance-interface-holding-v4-transferable-view-4631_>`_

  View for ``Transferable``\.

  .. _constr-daml-finance-interface-holding-v4-transferable-view-62264:

  `View <constr-daml-finance-interface-holding-v4-transferable-view-62264_>`_

    (no fields)

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-holding-v4-transferable-view-4631_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-holding-v4-transferable-view-4631_>`_

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_ `View <type-daml-finance-interface-holding-v4-transferable-view-4631_>`_

Functions
---------

.. _function-daml-finance-interface-holding-v4-transferable-transfer-48285:

`transfer <function-daml-finance-interface-holding-v4-transferable-transfer-48285_>`_
  \: `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-transferable-i-68214_>`_ \-\> `Transfer <type-daml-finance-interface-holding-v4-transferable-transfer-3593_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Transferable <type-daml-finance-interface-holding-v4-transferable-transferable-83505_>`_)
