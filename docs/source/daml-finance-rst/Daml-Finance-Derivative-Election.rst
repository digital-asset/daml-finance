.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-derivative-election-11001:

Module Daml.Finance.Derivative.Election
=======================================

Templates
---------

.. _type-daml-finance-derivative-election-election-47518:

**template** `Election <type-daml-finance-derivative-election-election-47518_>`_

  An election, such as the exercise of an option\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - elector
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Entity making the election\.
     * - counterparty
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Faces the ``elector`` in the ``Holding``\.
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The provider of the election is an entity that has the authority to process the election and create a new instrument version\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - instrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - The instrument to which the election applies\.
     * - amount
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - Number of units of instrument to which the election applies\.
     * - claim
       - :ref:`C <type-daml-finance-interface-derivative-types-c-63687>`
       - the elected sub\-tree\.
     * - electorIsOwner
       - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
       - ``True`` if the elector is the owner of a claim, ``False`` otherwise\.
     * - electionTime
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Time at which the election is put forward\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - A set of observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  + **implements** :ref:`I <type-daml-finance-interface-derivative-election-i-77182>`
  
  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

.. _type-daml-finance-derivative-election-electionfactory-92193:

**template** `ElectionFactory <type-daml-finance-derivative-election-electionfactory-92193_>`_

  Helper contract to delegate the right to create ``Election``\\s referencing a specific ``Instrument``\.
  The provider delegates the ability to create ``Election``\\s to any party that has visibility on the ``ElectionFactory`` contract\. In order to create the ``Election``, a valid ``Holding`` must be presented which identifies the choice controller as either the owner or the custodian to the ``Holding``\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - provider
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - a textual identifier
     * - claim
       - :ref:`C <type-daml-finance-interface-derivative-types-c-63687>`
       - the elected sub\-tree
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - observers of the contract
     * - instrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - key of the instrument to which the election applies
  
  + **Choice Archive**
    
  
  + **Choice CreateElection**
    
    Creates an ``Election`` on an instrument\. This choice must be exercised by the ``elector``\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - elector
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Entity making the election\.
       * - electionTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Time at which the election is put forward\.
       * - holdingCid
         - `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-holding-i-4221>`
         - A holding used to verify that the elector is entitled to make the election\.
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Number of units of instrument to which the election applies\.

Data Types
----------

.. _type-daml-finance-derivative-election-t-54254:

**type** `T <type-daml-finance-derivative-election-t-54254_>`_
  \= `Election <type-daml-finance-derivative-election-election-47518_>`_
  
  **instance** :ref:`HasImplementation <class-daml-finance-interface-derivative-election-hasimplementation-75558>` `T <type-daml-finance-derivative-election-t-54254_>`_
