.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-effect-31424:

Daml.Finance.Lifecycle.V4.Effect
================================

Templates
---------

.. _type-daml-finance-lifecycle-v4-effect-effect-15931:

**template** `Effect <type-daml-finance-lifecycle-v4-effect-effect-15931_>`_

  A contract encoding the consequences of a lifecycle event for one unit of the target
  instrument\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - The effect provider\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - The effect's identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The effect's description\.
     * - targetInstrument
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The target instrument\. A holding on this instrument is required to claim the effect\. For example, in the case of a swap instrument, this would be the original instrument version before lifecycling, that contains the current swap payment\.
     * - producedInstrument
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The produced instrument, if it exists\. For example, in the case of a swap instrument, this would be the new instrument version after lifecycling, that does not contain the current swap payment\. If there are no more claims remaining after the current lifecycling, this would be None\.
     * - otherConsumed
       - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
       - Consumed quantities (in addition to the target instrument)\. For example, in the case of a fix vs floating rate swap, this could be a 2\.5% fix payment\.
     * - otherProduced
       - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
       - Produced quantities (in additon to the produced instrument)\. For example, in the case of a fix vs floating rate swap, this could be a 3M Euribor floating payment\.
     * - settlementTime
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - The effect's settlement time (if any)\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-effect-i-48349>` **for** `Effect <type-daml-finance-lifecycle-v4-effect-effect-15931_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Effect <type-daml-finance-lifecycle-v4-effect-effect-15931_>`_

Data Types
----------

.. _type-daml-finance-lifecycle-v4-effect-t-28329:

**type** `T <type-daml-finance-lifecycle-v4-effect-t-28329_>`_
  \= `Effect <type-daml-finance-lifecycle-v4-effect-effect-15931_>`_

  Type synonym for ``Effect``\.
