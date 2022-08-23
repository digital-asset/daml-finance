.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-effect-16050:

Module Daml.Finance.Interface.Lifecycle.Effect
==============================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-effect-effect-69649:

**interface** `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_

  Interface for contracts exposing effects of lifecycling processes\.
  
  + **Choice Calculate**
    
    Given a holding, it calculates the instrument quantities to settle\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - actor
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party calculating the quantities to settle\.
       * - holdingCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`
         - The holding being targeted\. TODO this gives the effect provider visibility on the holding\. As an alternative, we could take the holding instrument \+ amount as input so that nothing is leaked
  
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
  
  + **Choice SetProvider**
    
    Set the provider of the effect\. The provider has visibility on all sub\-transactions triggered by ``Claim``\\ing an effect\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - newProvider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The new provider\.
  
  + **Method calculate \:** Calculate \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `CalculationResult <type-daml-finance-interface-lifecycle-effect-calculationresult-55343_>`_
    
    Implementation of the ``Calculate`` choice\.
  
  + **Method setProvider \:** SetProvider \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_)
    
    Implementation of the ``SetProvider`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-lifecycle-effect-hasimplementation-26488:

**class** `Implementation <type-daml-finance-interface-lifecycle-effect-implementation-16520_>`_ t \=\> `HasImplementation <class-daml-finance-interface-lifecycle-effect-hasimplementation-26488_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-lifecycle-effect-calculationresult-55343:

**data** `CalculationResult <type-daml-finance-interface-lifecycle-effect-calculationresult-55343_>`_

  Data type encapsulating the effect's calculation result\.
  
  .. _constr-daml-finance-interface-lifecycle-effect-calculationresult-87932:
  
  `CalculationResult <constr-daml-finance-interface-lifecycle-effect-calculationresult-87932_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - consumed
         - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-62956>`\]
         - Consumed quantities\.
       * - produced
         - \[:ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-62956>`\]
         - Produced quantities\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CalculationResult <type-daml-finance-interface-lifecycle-effect-calculationresult-55343_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CalculationResult <type-daml-finance-interface-lifecycle-effect-calculationresult-55343_>`_
  
  **instance** HasMethod `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_ \"calculate\" (Calculate \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `CalculationResult <type-daml-finance-interface-lifecycle-effect-calculationresult-55343_>`_)

.. _type-daml-finance-interface-lifecycle-effect-i-11106:

**type** `I <type-daml-finance-interface-lifecycle-effect-i-11106_>`_
  \= `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_
  
  **instance** HasMethod :ref:`Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711>` \"applyElection\" (ApplyElection \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Exercisable <type-daml-finance-interface-instrument-generic-election-exercisable-33711>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-effect-i-11106_>`_\]))
  
  **instance** HasMethod :ref:`Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497>` \"lifecycle\" (Lifecycle \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Lifecyclable <type-daml-finance-interface-lifecycle-lifecyclable-lifecyclable-83497>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-lifecycle-effect-i-11106_>`_\]))

.. _type-daml-finance-interface-lifecycle-effect-implementation-16520:

**type** `Implementation <type-daml-finance-interface-lifecycle-effect-implementation-16520_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-lifecycle-effect-i-11106_>`_
  
  Type constraint used to require templates implementing ``Effect`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-lifecycle-effect-v-39253:

**type** `V <type-daml-finance-interface-lifecycle-effect-v-39253_>`_
  \= `View <type-daml-finance-interface-lifecycle-effect-view-32411_>`_

.. _type-daml-finance-interface-lifecycle-effect-view-32411:

**data** `View <type-daml-finance-interface-lifecycle-effect-view-32411_>`_

  View for ``Effect``\.
  
  .. _constr-daml-finance-interface-lifecycle-effect-view-17386:
  
  `View <constr-daml-finance-interface-lifecycle-effect-view-17386_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party providing the claim processing\.
       * - targetInstrument
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-58546>`
         - A holding on this instrument is required to claim the effect\.
       * - producedInstrument
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-58546>`
         - The new version of the target instrument, when it exists\.
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual identifier\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-effect-view-32411_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-lifecycle-effect-view-32411_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-effect-view-32411_>`_

Functions
---------

.. _function-daml-finance-interface-lifecycle-effect-setprovider-26:

`setProvider <function-daml-finance-interface-lifecycle-effect-setprovider-26_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_ \=\> t \-\> SetProvider \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_)

.. _function-daml-finance-interface-lifecycle-effect-calculate-16959:

`calculate <function-daml-finance-interface-lifecycle-effect-calculate-16959_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_ \=\> t \-\> Calculate \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `Effect <type-daml-finance-interface-lifecycle-effect-effect-69649_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `CalculationResult <type-daml-finance-interface-lifecycle-effect-calculationresult-55343_>`_
