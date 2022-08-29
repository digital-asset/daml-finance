.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-settlement-batch-95573:

Module Daml.Finance.Settlement.Batch
====================================

Templates
---------

.. _type-daml-finance-settlement-batch-batch-59698:

**template** `Batch <type-daml-finance-settlement-batch-batch-59698_>`_

  Allows to atomically settle a set of settlement ``Step``\\s\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - requestors
       - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
       - Parties requesting the settlement\.
     * - settler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party triggering the settlement\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - stepsWithInstructionId
       - \[(:ref:`Step <type-daml-finance-interface-settlement-types-step-78661>`, `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_)\]
       - The settlement ``Step``\\s and the identifiers of the corresponding ``Instruction``\\s\.
  
  + **Choice Archive**
    

.. _type-daml-finance-settlement-batch-batchfactory-22649:

**template** `BatchFactory <type-daml-finance-settlement-batch-batchfactory-22649_>`_

  Factory template that implements the ``Instructable`` interface and is used to create a settlement batch\.
  A batch is made of a set of ``Instruction``\\s, as well as a container template used to atomically settle them\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party providing the facility to create settlement instructions\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
       - Observers\.
  
  + **Choice Archive**
    

.. _type-daml-finance-settlement-batch-batchfactorywithintermediaries-69374:

**template** `BatchFactoryWithIntermediaries <type-daml-finance-settlement-batch-batchfactorywithintermediaries-69374_>`_

  Factory template that implements the ``Instructable`` interface and is used to create a settlement batch\.
  A batch is made of a set of ``Instruction``\\s, as well as a container template used to atomically settle them\.
  For each instrument to settle as part of the batch, a hierarchy of intermediaries is specified in ``paths``\. This hierarchy is used to generate settlement instructions\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party providing the facility to create settlement instructions\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
       - Observers\.
     * - paths
       - `Map <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-map-90052>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ `Path <type-daml-finance-settlement-batch-path-29694_>`_
       - Hierarchical paths used to settle holding transfers\. A path is specified for each instrument label\.
  
  + **Choice Archive**
    

Data Types
----------

.. _type-daml-finance-settlement-batch-path-29694:

**data** `Path <type-daml-finance-settlement-batch-path-29694_>`_

  Data type that describes a hierarchical account structure between two parties for holdings on an instrument\.
  
  .. _constr-daml-finance-settlement-batch-path-26807:
  
  `Path <constr-daml-finance-settlement-batch-path-26807_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - senderPath
         - \[`Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_\]
         - Path from the sender to the root custodian of the holding\. If the sender ``p`` is the root custodian, it should be ``[p, p]``\.
       * - receiverPath
         - \[`Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_\]
         - Path from the receiver to the root custodian of the holding\. If the receiver ``p`` is the root custodian, it should be ``[p, p]``\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Path <type-daml-finance-settlement-batch-path-29694_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Path <type-daml-finance-settlement-batch-path-29694_>`_

.. _type-daml-finance-settlement-batch-t-36750:

**type** `T <type-daml-finance-settlement-batch-t-36750_>`_
  \= `Batch <type-daml-finance-settlement-batch-batch-59698_>`_
