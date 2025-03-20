.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-v4-instrument-7928:

Daml.Finance.Interface.Instrument.Generic.V4.Instrument
=======================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-generic-v4-instrument-instrument-84459:

**interface** `Instrument <type-daml-finance-interface-instrument-generic-v4-instrument-instrument-84459_>`_

  Interface for generic instruments utilizing Contingent Claims\.

  **viewtype** `V <type-daml-finance-interface-instrument-generic-v4-instrument-v-84255_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-generic-v4-instrument-getview-76480:

    **Choice** `GetView <type-daml-finance-interface-instrument-generic-v4-instrument-getview-76480_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-generic-v4-instrument-v-84255_>`_

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

.. _type-daml-finance-interface-instrument-generic-v4-instrument-i-8248:

**type** `I <type-daml-finance-interface-instrument-generic-v4-instrument-i-8248_>`_
  \= `Instrument <type-daml-finance-interface-instrument-generic-v4-instrument-instrument-84459_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-generic-v4-factory-factory-8845>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-generic-v4-factory-create-52332>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-generic-v4-instrument-i-8248_>`_))

.. _type-daml-finance-interface-instrument-generic-v4-instrument-v-84255:

**type** `V <type-daml-finance-interface-instrument-generic-v4-instrument-v-84255_>`_
  \= `View <type-daml-finance-interface-instrument-generic-v4-instrument-view-70325_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-generic-v4-instrument-instrument-84459_>`_ `V <type-daml-finance-interface-instrument-generic-v4-instrument-v-84255_>`_

.. _type-daml-finance-interface-instrument-generic-v4-instrument-view-70325:

**data** `View <type-daml-finance-interface-instrument-generic-v4-instrument-view-70325_>`_

  .. _constr-daml-finance-interface-instrument-generic-v4-instrument-view-58740:

  `View <constr-daml-finance-interface-instrument-generic-v4-instrument-view-58740_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument's key\.
       * - claims
         - :ref:`C <type-daml-finance-interface-claims-v4-types-c-76802>`
         - The claim tree\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-generic-v4-instrument-view-70325_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-generic-v4-instrument-view-70325_>`_
