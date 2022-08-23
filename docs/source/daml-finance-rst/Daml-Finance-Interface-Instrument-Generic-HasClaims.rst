.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-hasclaims-47920:

Module Daml.Finance.Interface.Instrument.Generic.HasClaims
==========================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-75942:

**interface** `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-75942_>`_

  Interface implemented by templates that admit a representation as a set of contingent claims\.
  
  + **Choice GetView**
    
    Retrieves the instrument view\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.
  
  + **Method getClaims \:** `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-22591>`\]
    
    Gets the set of claims representing the instrument\.

Data Types
----------

.. _type-daml-finance-interface-instrument-generic-hasclaims-i-36868:

**type** `I <type-daml-finance-interface-instrument-generic-hasclaims-i-36868_>`_
  \= `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-75942_>`_

.. _type-daml-finance-interface-instrument-generic-hasclaims-implementation-36294:

**type** `Implementation <type-daml-finance-interface-instrument-generic-hasclaims-implementation-36294_>`_ t
  \= `HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `I <type-daml-finance-interface-instrument-generic-hasclaims-i-36868_>`_
  
  Type constraint used to require templates implementing ``HasClaims`` to not
  require any other interface to be implemented\.

.. _type-daml-finance-interface-instrument-generic-hasclaims-v-8595:

**type** `V <type-daml-finance-interface-instrument-generic-hasclaims-v-8595_>`_
  \= `View <type-daml-finance-interface-instrument-generic-hasclaims-view-45529_>`_

.. _type-daml-finance-interface-instrument-generic-hasclaims-view-45529:

**data** `View <type-daml-finance-interface-instrument-generic-hasclaims-view-45529_>`_

  View for ``HasClaims``\.
  
  .. _constr-daml-finance-interface-instrument-generic-hasclaims-view-97008:
  
  `View <constr-daml-finance-interface-instrument-generic-hasclaims-view-97008_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - acquisitionTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The claim's acquisition time\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-generic-hasclaims-view-45529_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-generic-hasclaims-view-45529_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-generic-hasclaims-view-45529_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-generic-hasclaims-getclaims-12334:

`getClaims <function-daml-finance-interface-instrument-generic-hasclaims-getclaims-12334_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-75942_>`_ \=\> t \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-22591>`\]

.. _function-daml-finance-interface-instrument-generic-hasclaims-getclaim-14638:

`getClaim <function-daml-finance-interface-instrument-generic-hasclaims-getclaim-14638_>`_
  \: `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-75942_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ :ref:`C <type-daml-finance-interface-instrument-generic-types-c-8090>`
  
  Retrieves the single claim representing the template\. An error is thrown if there are zero or more than one claims\.

.. _function-daml-finance-interface-instrument-generic-hasclaims-getacquisitiontime-30342:

`getAcquisitionTime <function-daml-finance-interface-instrument-generic-hasclaims-getacquisitiontime-30342_>`_
  \: `HasClaims <type-daml-finance-interface-instrument-generic-hasclaims-hasclaims-75942_>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
  
  Retrieves the claim's acquisition time\.
