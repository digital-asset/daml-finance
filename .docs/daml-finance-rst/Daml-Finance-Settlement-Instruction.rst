.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-settlement-instruction-87187:

Module Daml.Finance.Settlement.Instruction
==========================================

Templates
---------

.. _type-daml-finance-settlement-instruction-instruction-35758:

**template** `Instruction <type-daml-finance-settlement-instruction-instruction-35758_>`_

  Instruction is used to settle a single settlement ``Step``\. In order to settle the instruction,
  
  * the sender must allocate a suitable transferable holding\.
  * the receiver must define the receiving account\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - requestors
       - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
       - Parties requesting the settlement\.
     * - signed
       - :ref:`Parties <type-daml-finance-interface-common-types-parties-45858>`
       - Additional signatories, used to collect authorization\.
     * - settler
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party triggering the settlement\.
     * - step
       - :ref:`Step <type-daml-finance-interface-settlement-types-step-78661>`
       - Settlement step\.
     * - allocation
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-asset-transferable-i-10374>`)
       - Allocated holding\.
     * - account
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`AccountKey <type-daml-finance-interface-asset-types-accountkey-21197>`
       - Receiving account\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual identifier\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
  
  + **implements** :ref:`I <type-daml-finance-interface-settlement-instruction-i-90342>`

Data Types
----------

.. _type-daml-finance-settlement-instruction-t-45988:

**type** `T <type-daml-finance-settlement-instruction-t-45988_>`_
  \= `Instruction <type-daml-finance-settlement-instruction-instruction-35758_>`_
