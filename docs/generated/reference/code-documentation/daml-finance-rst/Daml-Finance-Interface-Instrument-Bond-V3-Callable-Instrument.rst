.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-callable-instrument-14719:

Daml.Finance.Interface.Instrument.Bond.V3.Callable.Instrument
=============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-v3-callable-instrument-instrument-53544:

**interface** `Instrument <type-daml-finance-interface-instrument-bond-v3-callable-instrument-instrument-53544_>`_

  Instrument interface representing a callable bond\.

  **viewtype** `V <type-daml-finance-interface-instrument-bond-v3-callable-instrument-v-99854_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-bond-v3-callable-instrument-getview-84553:

    **Choice** `GetView <type-daml-finance-interface-instrument-bond-v3-callable-instrument-getview-84553_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-bond-v3-callable-instrument-v-99854_>`_

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

.. _type-daml-finance-interface-instrument-bond-v3-callable-instrument-i-23721:

**type** `I <type-daml-finance-interface-instrument-bond-v3-callable-instrument-i-23721_>`_
  \= `Instrument <type-daml-finance-interface-instrument-bond-v3-callable-instrument-instrument-53544_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-bond-v3-callable-factory-factory-95382>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-bond-v3-callable-factory-create-44265>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-bond-v3-callable-instrument-i-23721_>`_))

.. _type-daml-finance-interface-instrument-bond-v3-callable-instrument-v-99854:

**type** `V <type-daml-finance-interface-instrument-bond-v3-callable-instrument-v-99854_>`_
  \= `View <type-daml-finance-interface-instrument-bond-v3-callable-instrument-view-85850_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-bond-v3-callable-instrument-instrument-53544_>`_ `V <type-daml-finance-interface-instrument-bond-v3-callable-instrument-v-99854_>`_

.. _type-daml-finance-interface-instrument-bond-v3-callable-instrument-view-85850:

**data** `View <type-daml-finance-interface-instrument-bond-v3-callable-instrument-view-85850_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-bond-v3-callable-instrument-view-82591:

  `View <constr-daml-finance-interface-instrument-bond-v3-callable-instrument-view-82591_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - callable
         - :ref:`Callable <type-daml-finance-interface-instrument-bond-v3-callable-types-callable-12794>`
         - Attributes of a callable bond\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-v3-callable-instrument-view-85850_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-v3-callable-instrument-view-85850_>`_
