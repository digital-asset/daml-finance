.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-asset-holding-38531:

Module Daml.Finance.Interface.Asset.Holding
===========================================

Interfaces
----------

.. _type-daml-finance-interface-asset-holding-holding-42619:

**interface** `Holding <type-daml-finance-interface-asset-holding-holding-42619_>`_

  Base interface for a holding\.
  
  + **Choice GetView**
    
    Retrieve the holding view\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.
  
  + **Method asDisclosure \:** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
    
    Conversion to ``Disclosure`` interface\.

Typeclasses
-----------

.. _class-daml-finance-interface-asset-holding-hasimplementation-98201:

**class** `Implementation <type-daml-finance-interface-asset-holding-implementation-34045_>`_ t \=\> `HasImplementation <class-daml-finance-interface-asset-holding-hasimplementation-98201_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-asset-holding-i-4221:

**type** `I <type-daml-finance-interface-asset-holding-i-4221_>`_
  \= `Holding <type-daml-finance-interface-asset-holding-holding-42619_>`_
  
  Type synonym for ``Holding``\.
  
  **instance** HasMethod :ref:`Account <type-daml-finance-interface-asset-account-account-19971>` \"credit\" (Credit \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-holding-i-4221_>`_))
  
  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-asset-factory-holding-factory-96220>` \"create'\" (Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-asset-holding-i-4221_>`_))
  
  **instance** HasMethod :ref:`Lockable <type-daml-finance-interface-asset-lockable-lockable-65857>` \"asHolding\" `I <type-daml-finance-interface-asset-holding-i-4221_>`_

.. _type-daml-finance-interface-asset-holding-implementation-34045:

**type** `Implementation <type-daml-finance-interface-asset-holding-implementation-34045_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-asset-holding-i-4221_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Holding`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-asset-holding-v-6554:

**type** `V <type-daml-finance-interface-asset-holding-v-6554_>`_
  \= `View <type-daml-finance-interface-asset-holding-view-23126_>`_
  
  Type synonym for ``View``\.

.. _type-daml-finance-interface-asset-holding-view-23126:

**data** `View <type-daml-finance-interface-asset-holding-view-23126_>`_

  View for ``Holding``\.
  
  .. _constr-daml-finance-interface-asset-holding-view-5689:
  
  `View <constr-daml-finance-interface-asset-holding-view-5689_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
         - Instrument being held\.
       * - account
         - :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
         - Key of the account holding the assets\.
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Size of the holding\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-asset-holding-view-23126_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-asset-holding-view-23126_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-asset-holding-view-23126_>`_

Functions
---------

.. _function-daml-finance-interface-asset-holding-asdisclosure-67530:

`asDisclosure <function-daml-finance-interface-asset-holding-asdisclosure-67530_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Holding <type-daml-finance-interface-asset-holding-holding-42619_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
