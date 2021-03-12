# Contingent Claims

This is a library for modelling contingent claims i.e. derivatives. Briefly, a
derivative is represnted by a tree of `Claim`s, which describe the future
cashflows between two (implicit) parties.

The implementation follows closely the papers [[1]](#1), [[2]](#2).

To get started, look in the [test directory](./daml/Test) for examples how to
create and lifecycle contracts.

# License

Distributed under the Apache 2.0 License.

# Dependencies

There is a dependency on `Daml.Control.Recursion` which is currently provided
through a [git submodule](https://git-scm.com/book/en/v2/Git-Tools-Submodules).

# Building

First off, if `daml-ctl` directory is empty, remember to:

```
git submodule init && git submodule update
```

You can build a release version (no tests in the `*.dar`) by running `daml
build` in the root directory, or a dev version that includes test from
`daml-test`.

# References

<a id="1">[1]</a>
Jones, S. Peyton, Jean-Marc Eber, and Julian Seward.
"Composing contracts: an adventure in financial engineering."
ACM SIG-PLAN Notices 35.9 (2000): 280-292.

<a id="2">[2]</a>
Jones, SL Peyton, and J. M. Eber.
"How to write a financial contract",
volume "Fun Of Programming" of "Cornerstones of Computing." (2005).


