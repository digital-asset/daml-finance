.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-settlementrule-17558:

Module Daml.Finance.Interface.Lifecycle.SettlementRule
======================================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-settlementrule-settlementrule-62193:

**interface** `SettlementRule <type-daml-finance-interface-lifecycle-settlementrule-settlementrule-62193_>`_

  Interface for contracts that allow holders to claim an ``Effect`` and generate ``SettlementInstruction``\\s\.
  
  + **Choice Claim**
    
    Claim an effect and generate corresponding settlement instructions\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - claimer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party claiming the effect\.
       * - holdingCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`\]
         - The positions to process as part of the claim\.
       * - effectCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`
         - The effect to settle\.
  
  + **Choice GetView**
    
    Retrieves the instrument view\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.
  
  + **Method claim \:** Claim \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `ClaimResult <type-daml-finance-interface-lifecycle-settlementrule-claimresult-96002_>`_
    
    Implementation of the ``Claim`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-lifecycle-settlementrule-hasimplementation-29040:

**class** `Implementation <type-daml-finance-interface-lifecycle-settlementrule-implementation-14460_>`_ t \=\> `HasImplementation <class-daml-finance-interface-lifecycle-settlementrule-hasimplementation-29040_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-lifecycle-settlementrule-claimresult-96002:

**data** `ClaimResult <type-daml-finance-interface-lifecycle-settlementrule-claimresult-96002_>`_

  Data type wrapping the results of ``Claim``ing an ``Effect``\.
  
  .. _constr-daml-finance-interface-lifecycle-settlementrule-claimresult-93265:
  
  `ClaimResult <constr-daml-finance-interface-lifecycle-settlementrule-claimresult-93265_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - newInstrumentHoldingCids
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`\]
         - The holdings on the instrument produced by the effect, when it exists\.
       * - containerCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-settleable-i-95514>`
         - Container to batch\-settle settlement instructions\.
       * - instructionCids
         - \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-instruction-i-90342>`\]
         - Settlement instructions to settle all effect consequences other than consuming / upgrading the target instrument\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ClaimResult <type-daml-finance-interface-lifecycle-settlementrule-claimresult-96002_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ClaimResult <type-daml-finance-interface-lifecycle-settlementrule-claimresult-96002_>`_
  
  **instance** HasMethod `SettlementRule <type-daml-finance-interface-lifecycle-settlementrule-settlementrule-62193_>`_ \"claim\" (Claim \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `ClaimResult <type-daml-finance-interface-lifecycle-settlementrule-claimresult-96002_>`_)

.. _type-daml-finance-interface-lifecycle-settlementrule-i-11766:

**type** `I <type-daml-finance-interface-lifecycle-settlementrule-i-11766_>`_
  \= `SettlementRule <type-daml-finance-interface-lifecycle-settlementrule-settlementrule-62193_>`_

.. _type-daml-finance-interface-lifecycle-settlementrule-implementation-14460:

**type** `Implementation <type-daml-finance-interface-lifecycle-settlementrule-implementation-14460_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-lifecycle-settlementrule-i-11766_>`_
  
  Type constraint used to require templates implementing ``SettlementRule`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-lifecycle-settlementrule-v-44193:

**type** `V <type-daml-finance-interface-lifecycle-settlementrule-v-44193_>`_
  \= `View <type-daml-finance-interface-lifecycle-settlementrule-view-62327_>`_

.. _type-daml-finance-interface-lifecycle-settlementrule-view-62327:

**data** `View <type-daml-finance-interface-lifecycle-settlementrule-view-62327_>`_

  View for ``SettlementRule``\.
  
  .. _constr-daml-finance-interface-lifecycle-settlementrule-view-40618:
  
  `View <constr-daml-finance-interface-lifecycle-settlementrule-view-40618_>`_ ()
  
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-settlementrule-view-62327_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-lifecycle-settlementrule-view-62327_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-settlementrule-view-62327_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-settlementrule-claim-91351:

`claim <function-daml-finance-interface-lifecycle-settlementrule-claim-91351_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `SettlementRule <type-daml-finance-interface-lifecycle-settlementrule-settlementrule-62193_>`_ \=\> t \-\> Claim \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `ClaimResult <type-daml-finance-interface-lifecycle-settlementrule-claimresult-96002_>`_
