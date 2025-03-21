.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-effect-48507:

Daml.Finance.Interface.Lifecycle.V4.Effect
==========================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-effect-effect-33904:

**interface** `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_

  Interface for contracts exposing effects of lifecycling processes\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-effect-v-50682_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-effect-calculate-57344:

    **Choice** `Calculate <type-daml-finance-interface-lifecycle-v4-effect-calculate-57344_>`_

    Given an instrument quantity of the target instrument, it calculates the instrument
    quantities to settle\.

    Controller\: actor

    Returns\: `CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actor
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The flexible controller of the choice\.
       * - quantity
         - :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`
         - The quantity being targeted\.

  + .. _type-daml-finance-interface-lifecycle-v4-effect-getview-90853:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-effect-getview-90853_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-effect-view-53622_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-lifecycle-v4-effect-setproviders-39879:

    **Choice** `SetProviders <type-daml-finance-interface-lifecycle-v4-effect-setproviders-39879_>`_

    Set the provider of the effect\.

    Controller\: (DA\.Internal\.Record\.getField @\"providers\" (view this)), newProviders

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - newProviders
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The new provider\.

  + **Method calculate \:** `Calculate <type-daml-finance-interface-lifecycle-v4-effect-calculate-57344_>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392_>`_

    Implementation of the ``Calculate`` choice\.

  + **Method setProviders \:** `SetProviders <type-daml-finance-interface-lifecycle-v4-effect-setproviders-39879_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_)

    Implementation of the ``SetProviders`` choice\.

Data Types
----------

.. _type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392:

**data** `CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392_>`_

  Data type encapsulating the effect's calculation result\.

  .. _constr-daml-finance-interface-lifecycle-v4-effect-calculationresult-42457:

  `CalculationResult <constr-daml-finance-interface-lifecycle-v4-effect-calculationresult-42457_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - consumed
         - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
         - Consumed quantities\.
       * - produced
         - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
         - Produced quantities\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392_>`_

  **instance** HasMethod `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_ \"calculate\" (`Calculate <type-daml-finance-interface-lifecycle-v4-effect-calculate-57344_>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392_>`_)

.. _type-daml-finance-interface-lifecycle-v4-effect-i-48349:

**type** `I <type-daml-finance-interface-lifecycle-v4-effect-i-48349_>`_
  \= `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_

  Type synonym for ``Effect``\.

  **instance** HasMethod :ref:`Election <type-daml-finance-interface-lifecycle-v4-election-election-99800>` \"apply\" (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Election <type-daml-finance-interface-lifecycle-v4-election-election-99800>` \-\> :ref:`Apply <type-daml-finance-interface-lifecycle-v4-election-apply-6828>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-v4-effect-i-48349_>`_\]))

  **instance** HasMethod :ref:`Exercisable <type-daml-finance-interface-lifecycle-v4-election-exercisable-36259>` \"applyElection\" (:ref:`ApplyElection <type-daml-finance-interface-lifecycle-v4-election-applyelection-69809>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-v4-effect-i-48349_>`_\]))

  **instance** HasMethod :ref:`Lifecycle <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-lifecycle-50587>` \"evolve\" (:ref:`Evolve <type-daml-finance-interface-lifecycle-v4-rule-lifecycle-evolve-32221>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-v4-effect-i-48349_>`_\]))

.. _type-daml-finance-interface-lifecycle-v4-effect-v-50682:

**type** `V <type-daml-finance-interface-lifecycle-v4-effect-v-50682_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-effect-view-53622_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_ `V <type-daml-finance-interface-lifecycle-v4-effect-v-50682_>`_

.. _type-daml-finance-interface-lifecycle-v4-effect-view-53622:

**data** `View <type-daml-finance-interface-lifecycle-v4-effect-view-53622_>`_

  View for ``Effect``\.

  .. _constr-daml-finance-interface-lifecycle-v4-effect-view-76089:

  `View <constr-daml-finance-interface-lifecycle-v4-effect-view-76089_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - providers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The parties providing the claim processing\.
       * - targetInstrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The target instrument\. A holding on this instrument is required to claim the effect\. For example, in the case of a swap instrument, this would be the original instrument version before lifecycling, that contains the current swap payment\.
       * - producedInstrument
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The produced instrument, if it exists\. For example, in the case of a swap instrument, this would be the new instrument version after lifecycling, that does not contain the current swap payment\. If there are no more claims remaining after the current lifecycling, this would be None\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - A textual identifier\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A human readable description of the Effect\.
       * - settlementTime
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The effect's settlement time (if any)\.
       * - otherConsumed
         - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
         - Consumed quantities (in addition to the target instrument)\. For example, in the case of a fix vs floating rate swap, this could be a 2\.5% fix payment\.
       * - otherProduced
         - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
         - Produced quantities (in additon to the produced instrument)\. For example, in the case of a fix vs floating rate swap, this could be a 3M Euribor floating payment\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-effect-view-53622_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-effect-view-53622_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-v4-effect-setproviders-76675:

`setProviders <function-daml-finance-interface-lifecycle-v4-effect-setproviders-76675_>`_
  \: `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_ \-\> `SetProviders <type-daml-finance-interface-lifecycle-v4-effect-setproviders-39879_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_)

.. _function-daml-finance-interface-lifecycle-v4-effect-calculate-88708:

`calculate <function-daml-finance-interface-lifecycle-v4-effect-calculate-88708_>`_
  \: `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_ \-\> `Calculate <type-daml-finance-interface-lifecycle-v4-effect-calculate-57344_>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-v4-effect-effect-33904_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `CalculationResult <type-daml-finance-interface-lifecycle-v4-effect-calculationresult-17392_>`_
