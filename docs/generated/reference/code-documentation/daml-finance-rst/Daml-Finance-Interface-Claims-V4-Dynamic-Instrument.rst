.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-claims-v4-dynamic-instrument-86702:

Daml.Finance.Interface.Claims.V4.Dynamic.Instrument
===================================================

Interfaces
----------

.. _type-daml-finance-interface-claims-v4-dynamic-instrument-instrument-19765:

**interface** `Instrument <type-daml-finance-interface-claims-v4-dynamic-instrument-instrument-19765_>`_

  Interface implemented by instruments that create Contingent Claims trees on\-the\-fly (i\.e\., the
  tree is not stored on disk as part of a contract, but created and processed in\-memory)\.

  **viewtype** `V <type-daml-finance-interface-claims-v4-dynamic-instrument-v-26613_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-claims-v4-dynamic-instrument-createnewversion-58950:

    **Choice** `CreateNewVersion <type-daml-finance-interface-claims-v4-dynamic-instrument-createnewversion-58950_>`_

    Create a new version of an instrument, using a new lastEventTimestamp and a list of
    previous elections (if applicable)\.

    Controller\: (DA\.Internal\.Record\.getField @\"lifecycler\" (view this))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instrument <type-daml-finance-interface-claims-v4-dynamic-instrument-instrument-19765_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - version
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The new version of the instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The new lastEventTimestamp of the instrument\.
       * - prevEvents
         - \[EventData\]
         - A list of previous elections that have been lifecycled on this instrument so far\.

  + .. _type-daml-finance-interface-claims-v4-dynamic-instrument-getview-33666:

    **Choice** `GetView <type-daml-finance-interface-claims-v4-dynamic-instrument-getview-33666_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-claims-v4-dynamic-instrument-view-90779_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method createNewVersion \:** `CreateNewVersion <type-daml-finance-interface-claims-v4-dynamic-instrument-createnewversion-58950_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instrument <type-daml-finance-interface-claims-v4-dynamic-instrument-instrument-19765_>`_)

Data Types
----------

.. _type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466:

**type** `I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466_>`_
  \= `Instrument <type-daml-finance-interface-claims-v4-dynamic-instrument-instrument-19765_>`_

  Type synonym for ``Instrument``\.

.. _type-daml-finance-interface-claims-v4-dynamic-instrument-v-26613:

**type** `V <type-daml-finance-interface-claims-v4-dynamic-instrument-v-26613_>`_
  \= `View <type-daml-finance-interface-claims-v4-dynamic-instrument-view-90779_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-claims-v4-dynamic-instrument-instrument-19765_>`_ `V <type-daml-finance-interface-claims-v4-dynamic-instrument-v-26613_>`_

.. _type-daml-finance-interface-claims-v4-dynamic-instrument-view-90779:

**data** `View <type-daml-finance-interface-claims-v4-dynamic-instrument-view-90779_>`_

  View for ``Instrument``\.

  .. _constr-daml-finance-interface-claims-v4-dynamic-instrument-view-84562:

  `View <constr-daml-finance-interface-claims-v4-dynamic-instrument-view-84562_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - lifecycler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party performing the lifecycling\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - prevEvents
         - \[EventData\]
         - A list of previous elections that have been lifecycled on this instrument so far\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-claims-v4-dynamic-instrument-view-90779_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-claims-v4-dynamic-instrument-view-90779_>`_

Functions
---------

.. _function-daml-finance-interface-claims-v4-dynamic-instrument-createnewversion-59050:

`createNewVersion <function-daml-finance-interface-claims-v4-dynamic-instrument-createnewversion-59050_>`_
  \: `Instrument <type-daml-finance-interface-claims-v4-dynamic-instrument-instrument-19765_>`_ \-\> `CreateNewVersion <type-daml-finance-interface-claims-v4-dynamic-instrument-createnewversion-58950_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Instrument <type-daml-finance-interface-claims-v4-dynamic-instrument-instrument-19765_>`_)
