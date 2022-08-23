.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-asset-nontransferable-54395:

Module Daml.Finance.Asset.NonTransferable
=========================================

Templates
---------

.. _type-daml-finance-asset-nontransferable-factory-91184:

**template** `Factory <type-daml-finance-asset-nontransferable-factory-91184_>`_

  Implementation of the corresponding Holding Factory\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The factory's provider\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - The factory's observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`F <type-daml-finance-interface-asset-factory-holding-f-78374>`
  
  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _type-daml-finance-asset-nontransferable-nontransferable-14506:

**template** `NonTransferable <type-daml-finance-asset-nontransferable-nontransferable-14506_>`_

  Implementation of a non\-transferable holding\.
  ``NonTransferable`` implements the interface ``Lockable.I`` (which requires ``Holding.I``, and
  ``Disclosure.I`` to be implemented as well)\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - instrument
       - :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>`
       - The instrument of which units are held\.
     * - account
       - :ref:`AccountKey <type-daml-finance-interface-common-types-accountkey-81709>`
       - The account at which the holding is held\. Defines the holding's owner and custodian\.
     * - amount
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - Number of units\.
     * - lock
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-asset-lockable-lock-27785>`
       - An optional lock of a holding\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`
  
  + **implements** :ref:`I <type-daml-finance-interface-asset-lockable-i-23182>`
  
  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

Data Types
----------

.. _type-daml-finance-asset-nontransferable-f-27730:

**type** `F <type-daml-finance-asset-nontransferable-f-27730_>`_
  \= `Factory <type-daml-finance-asset-nontransferable-factory-91184_>`_

.. _type-daml-finance-asset-nontransferable-t-66900:

**type** `T <type-daml-finance-asset-nontransferable-t-66900_>`_
  \= `NonTransferable <type-daml-finance-asset-nontransferable-nontransferable-14506_>`_
  
  **instance** :ref:`HasImplementation <class-daml-finance-interface-asset-holding-hasimplementation-98201>` `T <type-daml-finance-asset-nontransferable-t-66900_>`_
