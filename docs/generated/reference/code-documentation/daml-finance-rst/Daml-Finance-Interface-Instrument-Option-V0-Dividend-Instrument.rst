.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-dividend-instrument-37029:

Daml.Finance.Interface.Instrument.Option.V0.Dividend.Instrument
===============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-option-v0-dividend-instrument-instrument-39714:

**interface** `Instrument <type-daml-finance-interface-instrument-option-v0-dividend-instrument-instrument-39714_>`_

  Instrument interface representing a physically settled Dividend option\.

  **viewtype** `V <type-daml-finance-interface-instrument-option-v0-dividend-instrument-v-12212_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-option-v0-dividend-instrument-getview-74827:

    **Choice** `GetView <type-daml-finance-interface-instrument-option-v0-dividend-instrument-getview-74827_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-option-v0-dividend-instrument-v-12212_>`_

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

.. _type-daml-finance-interface-instrument-option-v0-dividend-instrument-i-22979:

**type** `I <type-daml-finance-interface-instrument-option-v0-dividend-instrument-i-22979_>`_
  \= `Instrument <type-daml-finance-interface-instrument-option-v0-dividend-instrument-instrument-39714_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-option-v0-dividend-factory-factory-81964>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-option-v0-dividend-factory-create-44747>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-option-v0-dividend-instrument-i-22979_>`_))

.. _type-daml-finance-interface-instrument-option-v0-dividend-instrument-v-12212:

**type** `V <type-daml-finance-interface-instrument-option-v0-dividend-instrument-v-12212_>`_
  \= `View <type-daml-finance-interface-instrument-option-v0-dividend-instrument-view-15904_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-option-v0-dividend-instrument-instrument-39714_>`_ `V <type-daml-finance-interface-instrument-option-v0-dividend-instrument-v-12212_>`_

.. _type-daml-finance-interface-instrument-option-v0-dividend-instrument-view-15904:

**data** `View <type-daml-finance-interface-instrument-option-v0-dividend-instrument-view-15904_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-option-v0-dividend-instrument-view-62041:

  `View <constr-daml-finance-interface-instrument-option-v0-dividend-instrument-view-62041_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - dividend
         - :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>`
         - Attributes of a Dividend option\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-option-v0-dividend-instrument-view-15904_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-option-v0-dividend-instrument-view-15904_>`_
