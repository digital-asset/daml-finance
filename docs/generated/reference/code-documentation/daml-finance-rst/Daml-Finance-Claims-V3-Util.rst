.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-claims-v3-util-10150:

Daml.Finance.Claims.V3.Util
===========================

Functions
---------

.. _function-daml-finance-claims-v3-util-iszero-80616:

`isZero <function-daml-finance-claims-v3-util-iszero-80616_>`_
  \: `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Checks if all input claims are zero\.

.. _function-daml-finance-claims-v3-util-iszerotick-62164:

`isZero' <function-daml-finance-claims-v3-util-iszerotick-62164_>`_
  \: \[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\] \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Checks if all input claims are zero\.

.. _function-daml-finance-claims-v3-util-totime-8910:

`toTime <function-daml-finance-claims-v3-util-totime-8910_>`_
  \: (t \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` t x a o \-\> :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ x a o

  Maps the time parameter in a ``Claim`` to ``Time``\. As ``Time`` is generally understood to express
  UTC time, we recommend mapping to UTC time\.
