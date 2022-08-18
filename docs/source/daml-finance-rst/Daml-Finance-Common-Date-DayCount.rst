.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-common-date-daycount-33336:

Module Daml.Finance.Common.Date.DayCount
========================================

Data Types
----------

.. _type-daml-finance-common-date-daycount-daycountconventionenum-57741:

**data** `DayCountConventionEnum <type-daml-finance-common-date-daycount-daycountconventionenum-57741_>`_

  An enum type to specify a day count convention used to calculate day count fractions\.
  For a detailed definition of each convention, we refer to the \"Method of Interest Computation Indicator\" definitions in the context of the ISO\-20022 standard\. Where useful, we provide disambiguation comments\.
  
  .. _constr-daml-finance-common-date-daycount-act360-81384:
  
  `Act360 <constr-daml-finance-common-date-daycount-act360-81384_>`_
  
    Actual 360\.
  
  .. _constr-daml-finance-common-date-daycount-act365fixed-30756:
  
  `Act365_Fixed <constr-daml-finance-common-date-daycount-act365fixed-30756_>`_
  
    Actual 365 fixed\.
  
  .. _constr-daml-finance-common-date-daycount-basis30360-88837:
  
  `Basis_30360 <constr-daml-finance-common-date-daycount-basis30360-88837_>`_
  
    30/360 (also, 30/360 ISDA or A001 or American Basic rule)
  
  .. _constr-daml-finance-common-date-daycount-basis30360icma-30581:
  
  `Basis_30360_ICMA <constr-daml-finance-common-date-daycount-basis30360icma-30581_>`_
  
    30/360 ICMA (also, A011 or Basic Rule)\. This corresponds to \"30E/360\" of the 2006 ISDA definitions\.
  
  .. _constr-daml-finance-common-date-daycount-basis30e360-30153:
  
  `Basis_30E360 <constr-daml-finance-common-date-daycount-basis30e360-30153_>`_
  
    30E/360 (also, A007 or Eurobond basis)\. This corresponds to \"30E360 (ISDA)\" of the 2006 ISDA definitions\.
  
  .. _constr-daml-finance-common-date-daycount-basis30e3360-73543:
  
  `Basis_30E3360 <constr-daml-finance-common-date-daycount-basis30e3360-73543_>`_
  
    30E3/360 (also, A013 or Eurobond basis model 3)\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `DayCountConventionEnum <type-daml-finance-common-date-daycount-daycountconventionenum-57741_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `DayCountConventionEnum <type-daml-finance-common-date-daycount-daycountconventionenum-57741_>`_

Functions
---------

.. _function-daml-finance-common-date-daycount-calcdcf-71909:

`calcDcf <function-daml-finance-common-date-daycount-calcdcf-71909_>`_
  \: `DayCountConventionEnum <type-daml-finance-common-date-daycount-daycountconventionenum-57741_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
  
  Calculates the day count fraction given the correponding convention\.
  Currently 30E360 is not supported as we do not want to expose the maturity date of the product as an additional parameter\.

.. _function-daml-finance-common-date-daycount-calcdcf30e360-81320:

`calcDcf30E360 <function-daml-finance-common-date-daycount-calcdcf30e360-81320_>`_
  \: `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
  
  Calculate 30E/360 day count fraction\.
