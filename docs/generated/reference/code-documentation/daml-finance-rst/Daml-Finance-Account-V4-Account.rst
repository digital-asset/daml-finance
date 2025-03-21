.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-account-v4-account-5834:

Daml.Finance.Account.V4.Account
===============================

Templates
---------

.. _type-daml-finance-account-v4-account-account-35720:

**template** `Account <type-daml-finance-account-v4-account-account-35720_>`_

  A relationship between a custodian and an asset owner\. It is referenced by holdings\.

  Signatory\: custodian, owner, Lockable\.getLockers this

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
     * - lock
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Lock <type-daml-finance-interface-util-v3-lockable-lock-18728>`
       - An optional lock for the account\.
     * - controllers
       - :ref:`Controllers <type-daml-finance-interface-account-v4-account-controllers-59817>`
       - Controllers of transfers\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Identifier of the account\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Description of the account\.
     * - holdingFactory
       - :ref:`HoldingFactoryKey <type-daml-finance-interface-types-common-v3-types-holdingfactorykey-40007>`
       - Associated holding factory\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.

  + **Choice** Archive

    Controller\: custodian, owner, Lockable\.getLockers this

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-account-v4-account-i-22897>` **for** `Account <type-daml-finance-account-v4-account-account-35720_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Account <type-daml-finance-account-v4-account-account-35720_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-lockable-i-3709>` **for** `Account <type-daml-finance-account-v4-account-account-35720_>`_

.. _type-daml-finance-account-v4-account-factory-19307:

**template** `Factory <type-daml-finance-account-v4-account-factory-19307_>`_

  Template used to create accounts\.

  Signatory\: provider

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
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The factory's observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-account-v4-factory-i-68866>` **for** `Factory <type-daml-finance-account-v4-account-factory-19307_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-account-v4-account-factory-19307_>`_

Data Types
----------

.. _type-daml-finance-account-v4-account-t-56807:

**type** `T <type-daml-finance-account-v4-account-t-56807_>`_
  \= `Account <type-daml-finance-account-v4-account-account-35720_>`_

  Type synonym for ``Account``\.
