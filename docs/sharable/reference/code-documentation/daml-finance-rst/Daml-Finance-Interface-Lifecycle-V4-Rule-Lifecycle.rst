.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-rule-lifecycle-8270:

Daml.Finance.Interface.Lifecycle.V4.Rule.Lifecycle
==================================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-rule-lifecycle-lifecycle-50587:

**interface** `Lifecycle <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-lifecycle-50587_>`_

  Interface implemented by instruments that can be lifecycled (either by the instrument itself
  or by a separate rule contract)\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-v-43493_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221:

    **Choice** `Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221_>`_

    Process an event\. It returns a tuple of the lifecycled instrument (or the original
    instrument when the former does not exist) and the effects\.

    Controller\: (DA\.Internal\.Record\.getField @\"lifecycler\" (view this))

    Returns\: (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - eventCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>`
         - The event\.
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The target instrument\.
       * - observableCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-observable-numericobservable-i-61855>`\]
         - Set of numerical time\-dependent observables\.

  + .. _type-daml-finance-interface-lifecycle-v4-rule-lifecycle-getview-32882:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-getview-32882_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-1867_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + **Method evolve \:** `Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])

    Implementation of the ``Evolve`` choice\.

Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-rule-lifecycle-i-54386:

**type** `I <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-i-54386_>`_
  \= `Lifecycle <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-lifecycle-50587_>`_

  Type synonym for ``Lifecycle``\.

.. _type-daml-finance-interface-lifecycle-v4-rule-lifecycle-v-43493:

**type** `V <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-v-43493_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-1867_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Lifecycle <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-lifecycle-50587_>`_ `V <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-v-43493_>`_

.. _type-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-1867:

**data** `View <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-1867_>`_

  View for ``Lifecycle``\.

  .. _constr-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-51740:

  `View <constr-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-51740_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Identifier for the rule contract\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Textual description\.
       * - lifecycler
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party performing the lifecycling\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-1867_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-view-1867_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-27465:

`evolve <function-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-27465_>`_
  \: `Lifecycle <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-lifecycle-50587_>`_ \-\> `Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>`\])
