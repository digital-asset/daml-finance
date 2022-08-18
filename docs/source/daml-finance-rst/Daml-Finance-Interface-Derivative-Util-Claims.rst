.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-derivative-util-claims-76683:

Module Daml.Finance.Interface.Derivative.Util.Claims
====================================================

Functions
---------

.. _function-daml-finance-interface-derivative-util-claims-iszero-14361:

`isZero <function-daml-finance-interface-derivative-util-claims-iszero-14361_>`_
  \: :ref:`I <type-daml-finance-interface-derivative-hasclaims-i-90893>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
  
  Checks if all input claims are zero\.

.. _function-daml-finance-interface-derivative-util-claims-iszerotick-13075:

`isZero' <function-daml-finance-interface-derivative-util-claims-iszerotick-13075_>`_
  \: \[:ref:`TaggedClaim <type-daml-finance-interface-derivative-types-taggedclaim-29758>`\] \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
  
  Checks if all input claims are zero\.

.. _function-daml-finance-interface-derivative-util-claims-totime-6167:

`toTime <function-daml-finance-interface-derivative-util-claims-totime-6167_>`_
  \: :ref:`HasUTCTimeConversion <class-daml-finance-interface-common-classes-hasutctimeconversion-72400>` t \=\> Claim t x a o \-\> Claim `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ x a o
  
  Maps the time parameter in a ``Claim`` to ``Time``\. As ``Time`` is generally understood to express UTC time, we recommend mapping to UTC time\.

.. _function-daml-finance-interface-derivative-util-claims-totimetick-11737:

`toTime' <function-daml-finance-interface-derivative-util-claims-totimetick-11737_>`_
  \: (t \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> Claim t x a o \-\> Claim `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ x a o
  
  Maps the time parameter in a ``Claim`` to ``Time``\. As ``Time`` is generally understood to express UTC time, we recommend mapping to UTC time\.
