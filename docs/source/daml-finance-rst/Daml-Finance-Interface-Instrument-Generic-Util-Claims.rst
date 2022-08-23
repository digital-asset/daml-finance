.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-util-claims-70604:

Module Daml.Finance.Interface.Instrument.Generic.Util.Claims
============================================================

Functions
---------

.. _function-daml-finance-interface-instrument-generic-util-claims-iszero-55102:

`isZero <function-daml-finance-interface-instrument-generic-util-claims-iszero-55102_>`_
  \: :ref:`I <type-daml-finance-interface-instrument-generic-hasclaims-i-36868>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
  
  Checks if all input claims are zero\.

.. _function-daml-finance-interface-instrument-generic-util-claims-iszerotick-98594:

`isZero' <function-daml-finance-interface-instrument-generic-util-claims-iszerotick-98594_>`_
  \: \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-22591>`\] \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
  
  Checks if all input claims are zero\.

.. _function-daml-finance-interface-instrument-generic-util-claims-totime-18424:

`toTime <function-daml-finance-interface-instrument-generic-util-claims-totime-18424_>`_
  \: :ref:`HasUTCTimeConversion <class-daml-finance-interface-common-classes-hasutctimeconversion-72400>` t \=\> Claim t x a o \-\> Claim `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ x a o
  
  Maps the time parameter in a ``Claim`` to ``Time``\. As ``Time`` is generally understood to express UTC time, we recommend mapping to UTC time\.

.. _function-daml-finance-interface-instrument-generic-util-claims-totimetick-89828:

`toTime' <function-daml-finance-interface-instrument-generic-util-claims-totimetick-89828_>`_
  \: (t \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> Claim t x a o \-\> Claim `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_ x a o
  
  Maps the time parameter in a ``Claim`` to ``Time``\. As ``Time`` is generally understood to express UTC time, we recommend mapping to UTC time\.
