.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-rule-util-70932:

Daml.Finance.Lifecycle.V4.Rule.Util
===================================

Data Types
----------

.. _type-daml-finance-lifecycle-v4-rule-util-pending-30518:

**data** `Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518_>`_

  Type used to record pending payments\.

  .. _constr-daml-finance-lifecycle-v4-rule-util-pending-64409:

  `Pending <constr-daml-finance-lifecycle-v4-rule-util-pending-64409_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         -
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518_>`_

Functions
---------

.. _function-daml-finance-lifecycle-v4-rule-util-mergeconsumedandproduced-58739:

`mergeConsumedAndProduced <function-daml-finance-lifecycle-v4-rule-util-mergeconsumedandproduced-58739_>`_
  \: \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\] \-\> \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\] \-\> \[`Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518_>`_\]

  Merge consumed and produced instruments into a list of pending settlements\.
  This will only reproduce instrument and quantity, not tag or time\.

.. _function-daml-finance-lifecycle-v4-rule-util-splitpending-50551:

`splitPending <function-daml-finance-lifecycle-v4-rule-util-splitpending-50551_>`_
  \: \[`Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518_>`_\] \-\> (\[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\], \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\])

  Map pending settlements into corresponding instrument quantities and split them into consumed
  and produced\. Pending items with an amount of ``0.0`` are discarded\.

.. _function-daml-finance-lifecycle-v4-rule-util-net-10510:

`net <function-daml-finance-lifecycle-v4-rule-util-net-10510_>`_
  \: \[`Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518_>`_\] \-\> \[`Pending <type-daml-finance-lifecycle-v4-rule-util-pending-30518_>`_\]

  Net pending payments on the same instrument (regardless of tags)\.
