.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-token-v4-instrument-40238:

Daml.Finance.Interface.Instrument.Token.V4.Instrument
=====================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-token-v4-instrument-instrument-75613:

**interface** `Instrument <type-daml-finance-interface-instrument-token-v4-instrument-instrument-75613_>`_

  Interface for a Token, an instrument whose economic terms on the ledger are represented
  by an ``id`` and a textual ``description``\.

  **viewtype** `V <type-daml-finance-interface-instrument-token-v4-instrument-v-42717_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-token-v4-instrument-getview-61626:

    **Choice** `GetView <type-daml-finance-interface-instrument-token-v4-instrument-getview-61626_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-token-v4-instrument-v-42717_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.


Data Types
----------

.. _type-daml-finance-interface-instrument-token-v4-instrument-i-45050:

**type** `I <type-daml-finance-interface-instrument-token-v4-instrument-i-45050_>`_
  \= `Instrument <type-daml-finance-interface-instrument-token-v4-instrument-instrument-75613_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-token-v4-factory-factory-54831>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-token-v4-factory-create-20178>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-token-v4-instrument-i-45050_>`_))

.. _type-daml-finance-interface-instrument-token-v4-instrument-v-42717:

**type** `V <type-daml-finance-interface-instrument-token-v4-instrument-v-42717_>`_
  \= `View <type-daml-finance-interface-instrument-token-v4-instrument-view-88163_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-token-v4-instrument-instrument-75613_>`_ `V <type-daml-finance-interface-instrument-token-v4-instrument-v-42717_>`_

.. _type-daml-finance-interface-instrument-token-v4-instrument-view-88163:

**data** `View <type-daml-finance-interface-instrument-token-v4-instrument-view-88163_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-token-v4-instrument-view-89686:

  `View <constr-daml-finance-interface-instrument-token-v4-instrument-view-89686_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - token
         - :ref:`Token <type-daml-finance-interface-instrument-token-v4-types-token-51711>`
         - Attributes of a Token Instrument\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-token-v4-instrument-view-88163_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-token-v4-instrument-view-88163_>`_
