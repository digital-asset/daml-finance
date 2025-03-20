.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-holding-v4-baseholding-28133:

Daml.Finance.Holding.V4.BaseHolding
===================================

Templates
---------

.. _type-daml-finance-holding-v4-baseholding-baseholding-18612:

**template** `BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612_>`_

  Implementation of a ``BaseHolding``\.
  ``BaseHolding`` implements the interface ``Holding.I`` (which requires ``Lockable.I`` and
  ``Disclosure.I`` to be implemented), but neither ``Fungible.I`` nor ``Transferable.I``\.

  Signatory\: (DA\.Internal\.Record\.getField @\"custodian\" account), (DA\.Internal\.Record\.getField @\"owner\" account), Lockable\.getLockers this

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - instrument
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The instrument of which units are held\.
     * - account
       - :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`
       - The account at which the holding is held\. Defines the holding's owner and custodian\.
     * - amount
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - Number of units\.
     * - lock
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>`
       - An optional lock for the holding\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.

  + **Choice** Archive

    Controller\: (DA\.Internal\.Record\.getField @\"custodian\" account), (DA\.Internal\.Record\.getField @\"owner\" account), Lockable\.getLockers this

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>` **for** `BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-lockable-i-3709>` **for** `BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612_>`_

Data Types
----------

.. _type-daml-finance-holding-v4-baseholding-t-71378:

**type** `T <type-daml-finance-holding-v4-baseholding-t-71378_>`_
  \= `BaseHolding <type-daml-finance-holding-v4-baseholding-baseholding-18612_>`_

  Type synonym for ``BaseHolding``\.
