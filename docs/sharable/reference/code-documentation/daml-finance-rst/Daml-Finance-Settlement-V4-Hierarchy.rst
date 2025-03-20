.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-settlement-v4-hierarchy-83331:

Daml.Finance.Settlement.V4.Hierarchy
====================================

Data Types
----------

.. _type-daml-finance-settlement-v4-hierarchy-hierarchy-41337:

**data** `Hierarchy <type-daml-finance-settlement-v4-hierarchy-hierarchy-41337_>`_

  Data type that describes a hierarchical account structure among multiple parties for holdings
  on an instrument\.

  .. _constr-daml-finance-settlement-v4-hierarchy-hierarchy-81120:

  `Hierarchy <constr-daml-finance-settlement-v4-hierarchy-hierarchy-81120_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - rootCustodian
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Root custodian of the instrument\.
       * - pathsToRootCustodian
         - \[\[`Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_\]\]
         - Paths from \"leaf\" owners to the root custodian of the instrument\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Hierarchy <type-daml-finance-settlement-v4-hierarchy-hierarchy-41337_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Hierarchy <type-daml-finance-settlement-v4-hierarchy-hierarchy-41337_>`_

Functions
---------

.. _function-daml-finance-settlement-v4-hierarchy-unfoldstep-66521:

`unfoldStep <function-daml-finance-settlement-v4-hierarchy-unfoldstep-66521_>`_
  \: `Hierarchy <type-daml-finance-settlement-v4-hierarchy-hierarchy-41337_>`_ \-\> :ref:`Step <type-daml-finance-interface-settlement-v4-types-step-16302>` \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ \[:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`\]
