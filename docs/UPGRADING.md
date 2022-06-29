# Upgrading

This document contains upgrade instructions for major (and other significant) versions

## 2.0.0

* The SDK verison is now ≥ 2.0
* The dependency [daml-ctl](https://github.com/digital-asset/daml-ctl) is no longer a git-submodule. You need to [download it](https://github.com/digital-asset/daml-ctl/releases) and drop into `lib/`. Take care if building from source that the symlinks `daml/Daml` and `test/daml/Daml` are removed, and `daml-ctl` in the root dir also.
* `Claim f t x a` now becomes `Claim t x a`, which replaces `Claim.Serializable`. i.e. it does away with `{de}serialize` altogether. You'll probably need to update your function signatures, although implementations should stay the same.
* As result of the above, infix notation is broken so you can no longer write e.g.

```haskell
a `And` b `And` c
```

instead, you should now use lower-case smart constructors i.e.

```haskell
import Prelude hiding (and)

a `and` b `and` c
```
Beware that infix notation is inefficient - consider using the new `Monoid` instance `mconcat` when necessary. Better yet, use the higher-level financial primitives in `ContingentClaims.Financial` module instead.
