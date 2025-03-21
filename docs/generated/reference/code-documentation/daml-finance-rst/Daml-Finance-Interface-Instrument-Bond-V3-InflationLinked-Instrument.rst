.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-29769:

Daml.Finance.Interface.Instrument.Bond.V3.InflationLinked.Instrument
====================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-instrument-58178:

**interface** `Instrument <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-instrument-58178_>`_

  Instrument interface representing an inflation linked bond\.

  **viewtype** `V <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-v-15508_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-getview-17323:

    **Choice** `GetView <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-getview-17323_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-v-15508_>`_

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

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-i-26275:

**type** `I <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-i-26275_>`_
  \= `Instrument <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-instrument-58178_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-factory-33440>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-create-17927>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-i-26275_>`_))

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-v-15508:

**type** `V <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-v-15508_>`_
  \= `View <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-view-44800_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-instrument-58178_>`_ `V <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-v-15508_>`_

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-view-44800:

**data** `View <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-view-44800_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-view-83247:

  `View <constr-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-view-83247_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - inflationLinked
         - :ref:`InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736>`
         - Attributes of an inflation linked bond\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-view-44800_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-view-44800_>`_
