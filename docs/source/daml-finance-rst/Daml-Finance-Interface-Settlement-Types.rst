.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-settlement-types-44085:

Module Daml.Finance.Interface.Settlement.Types
==============================================

Data Types
----------

.. _type-daml-finance-interface-settlement-types-step-78661:

**data** `Step <type-daml-finance-interface-settlement-types-step-78661_>`_

  Describes a transfer of a position between two parties\.
  
  .. _constr-daml-finance-interface-settlement-types-step-97764:
  
  `Step <constr-daml-finance-interface-settlement-types-step-97764_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - sender
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party transferring the asset\.
       * - receiver
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party receiving the asset\.
       * - quantity
         - :ref:`Q <type-daml-finance-interface-instrument-base-instrument-q-62956>`
         - The instrument and amount to be transferred\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Step <type-daml-finance-interface-settlement-types-step-78661_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Step <type-daml-finance-interface-settlement-types-step-78661_>`_
