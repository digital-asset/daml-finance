.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-asset-account-76684:

Module Daml.Finance.Asset.Account
=================================

Templates
---------

.. _type-daml-finance-asset-account-account-64286:

**template** `Account <type-daml-finance-asset-account-account-64286_>`_

  A relationship between a custodian and an asset owner\. It is referenced by holdings\.

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - custodian
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The account provider\.
     * - owner
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The account owner\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - holdingFactoryCid
       - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`F <type-daml-finance-interface-asset-factory-holding-f-78374>`
       - Associated holding factory\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.

  + **Choice Archive**


  + **implements** :ref:`I <type-daml-finance-interface-asset-account-i-38237>`

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _type-daml-finance-asset-account-factory-10857:

**template** `Factory <type-daml-finance-asset-account-factory-10857_>`_

  Template used to create accounts\.

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


  + **implements** :ref:`F <type-daml-finance-interface-asset-factory-account-f-54942>`

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

Data Types
----------

.. _type-daml-finance-asset-account-t-52313:

**type** `T <type-daml-finance-asset-account-t-52313_>`_
  \= `Account <type-daml-finance-asset-account-account-64286_>`_

  **instance** :ref:`HasImplementation <class-daml-finance-interface-asset-account-hasimplementation-58285>` `T <type-daml-finance-asset-account-t-52313_>`_
