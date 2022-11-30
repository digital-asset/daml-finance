.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Contingent Claims Tree on Ledger vs On-the-Fly Creation
#######################################################

Different Ways to Create and Store the Contingent Claims Tree
*************************************************************

We have seen two different ways of modeling a fixed coupon bond using
:doc:`Contingent Claims <../../concepts/contingent-claims>`:

Explicitly Storing the Contingent Claims Tree on the Ledger
===========================================================

When we use the :doc:`Generic extension <generic-extension>`, we create the claims tree at
instrument inception and store this representation explicitly on the ledger. Since the tree is
stored statically it can only change if the instrument is updated on ledger. For example, after a
coupon payment a new version of the instrument (excluding the coupon just paid) supersedes the
previous version. However, in the event of a change in a holiday calendar (which could be used to
define the coupon payment dates), the tree will not automatically change.

Calculating the Contingent Claims Tree On-the-Fly
=================================================

In contrast, when we create a :doc:`strongly typed bond instrument <contingent-claims-instrument>`,
only the key parameters of the bond are stored on the ledger. The claims tree is not, it is created
on-the-fly when needed (for example, in the case of lifecycling). Consequently, if a holiday
calendar changes, this will automatically impact the claims tree the next time it is dynamically
created.

Which Is Preferred?
*******************

Both options are possible, this is more a matter of personal preference. They both have their pros
and cons.

The on-the-fly approach has the advantage that the claims tree can adapt to changes in reference
data like holiday calendars. Also, if the economic terms of the instrument would result in a very
large claims tree it could be desirable not to store it on the ledger for performance reasons.

On the other hand, if you need to quickly create a one-off instrument, the on ledger approach allows
you to create the claims directly from a script, without first having to define a dedicated
template. Also, if the :doc:`Contingent Claims <../../concepts/contingent-claims>` representation
is actively used by both counterparties of the trade it could be useful to have it on ledger from a
transparancy point of view. Similarly, if you need to explicitly keep the
:doc:`Contingent Claims <../../concepts/contingent-claims>` representations of older versions of
the instrument on the ledger, for example for auditing reasons, that would be achieved out of the
box.
