.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-util-v4-date-rollconvention-61455:

Daml.Finance.Util.V4.Date.RollConvention
========================================

Functions
---------

.. _function-daml-finance-util-v4-date-rollconvention-next-12170:

`next <function-daml-finance-util-v4-date-rollconvention-next-12170_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990>` \-\> :ref:`RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Get next periodic (daily ``D`` and weekly ``W`` not supported) date according
  to a given roll convention\.

.. _function-daml-finance-util-v4-date-rollconvention-previous-51738:

`previous <function-daml-finance-util-v4-date-rollconvention-previous-51738_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990>` \-\> :ref:`RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Get previous periodic (daily ``D`` and weekly ``W`` not supported) date according
  to a given roll convention\.

.. _function-daml-finance-util-v4-date-rollconvention-addperiod-52786:

`addPeriod <function-daml-finance-util-v4-date-rollconvention-addperiod-52786_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Add period to given date\.
