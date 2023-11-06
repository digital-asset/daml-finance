# Style Guide

This style guide details the formatting and styling of Daml and client-side code that is contained
within this repository.

## Daml

### Spacing

- Use 2 spaces for indentation
- Use single empty line to separate sections (and end the file with a single empty line)
- Use at most 100 characters per line. As a visual guide, you can add `"editor.rulers": [100]` to
  your vscode `settings.json`.

### Imports

- Order imports alphabetically with upper-case before lower-case:

  ```haskell
  import DA.Set (Set, empty)
  import Daml.Script
  ```

- Use qualified imports for interface modules:

  ```haskell
  import Daml.Finance.Interface.Asset.Holding qualified as Holding (view, I, V)
  ```

- Use explicit imports and order them alphabetically:

  In case of no name clashes, use unqualified imports:

  ```haskell
  import DA.Set (fromList, singleton)
  ```

  and otherwise qualified imports:

  ```haskell
  import DA.Map qualified as M (fromList)
  import DA.Set qualified as S (fromList, null)
  ```

### Exports

- Only use explicit export lists when not all types or functions in a given module are exported
- Use the `-- | HIDE` annotation for anything that is not exported
- Do add comments for types and functions that are not exported
- If you want an internal function to be tested individually, add it to the export list but
  annotate it with the `-- | HIDE` comment.

### Module structure

- Modules use the following newlines:

  ```haskell
  module Foo.Bar where

  import A.B.C
  import A.B.D

  template Baz
    with
      ...

  template Baz2
    with
      ...
    ```

### Template structure

- Templates use the following indentation and newlines:

  ```haskell
  template Foo
    with
      party1 : Party
      party2 : Party
    where
      signatory party1
      observer party2

      key party1 : Party
      maintainer party1

      let
        a = 1
        b = 2

      choice Delete : ()
        with
          party : Party
        controller party
        do
          pure ()
  ```

### Interface structure

- Group all serializable interface fields into a `View` type:

  ```haskell
  -- | View for `Account`.
  -- This data type is ..
  data View = View
    with
      custodian : Party
        -- ^ Party providing accounting
        --   services.
      id : Text
        -- ^ Textual description of
        --   the account.
      owner : Party
        -- ^ Party owning
        --   this account.
    deriving (Eq, Show)
  ```

- Add a `viewtype` definition to the interface with the `View` type:

  ```haskell
  interface Account where
    viewtype View
    ...
  ```

- Define `I` and `V` type aliases for the interface and view type, respectively:

  ```haskell
  type I = Account
  type V = View
  ```

- In consuming code, use qualified imports to refer to interfaces and view typs, e.g.,
  `import Daml.Finance.Interface.Holding qualified as Holding (I, V)`.

- For interface choices with empty implemenations

  To prevent us from keeping the implementation arguments 'in sync' with the choice arguments, we
  use

  ```haskell
  foo : Foo -> Update res
  choice Foo : res
    with
      a : A
      b : B
    controller actor
    do
      foo this arg
  ```

  instead of

  ```haskell
  foo : A -> B -> Update res
  choice Foo : res
    with
      a : A
      b : B
    controller actor
    do
      foo a b
  ```

  Recall that `arg` is syntactic sugar for `Foo with a; b` when used in the body of the choice.

### `do` notation

- Place the main `do` in a choice on its own line:

  ```haskell
  choice Delete : ()
    controller party
    do
      pure ()
  ```

- Place `do` in functions and expressions on the same line:

  ```haskell
  let
    foo = do
      if a
      then do
        ...
      else do
        ...
  ```

### Naming convention

- Use the CamlCase naming convention for types, variables, and functions. For example:

  ```haskell
  data MyOptions
    = OptionOne
    | OptionTwo
    | OptionThree
    deriving (Eq, Ord)
  ```

  ```haskell
  mySum : Decimal -> Decimal -> Decimal
  mySum a b = a + b
  ```

  ```haskell
  myTerm = 3
  ```

- Use plural 's' when naming `Set Party`.

### `let` bindings

- Single-line body:

  ```haskell
  let a = b
  ```

- Multi-line body:

  ```haskell
  let
    a =
      if b
      then c
      else d
  ```

- Multiple bindings:

  ```haskell
  let
    a = b
    c = d
  ```

### Flexible controllers

In case of flexible controllers, e.g.,

  ```haskell
    choice Foo : ()
      with
        actor : Party
        bar : Int
      controller actor
      do
        pure ()
  ```

we let the `actor : Party` be the first argument of the choice.

### Functions

- Add function type signatures to all top-level functions:

  ```haskell
  add : Decimal -> Decimal -> Decimal
  add a b = a + b
  ```

- Don't add type signatures for functions in `let` bindings (unless you think it is necessary):

  ```haskell
  let add a b = a + b
  ```

- Use `pure` instead of `return` as more idiomatic

### Constructors

- Use `;` to separate parameters to differentiate from list and tuple separators:

  ```haskell
  let foo = Foo with a = b; c = [d, e]; f = (g, h)
  ```

- Use short-hand parameter notation:

  ```haskell
  let foo = Foo with a; b; c
  ```

- Don't use `..` as it makes code harder to understand (and might cause bugs in case of code
  changes)

- Don't use positional constructors as they are harder to read (and might cause bugs in case of code
  changes)

### Some useful regex

In VS Code you can search using regex expressions. Here are some useful regex expressions to help
enforce some of the points in this style guide:

- `\w\s{2}\w` finds 2 whitespaces between words.

- `\n{2,}$` finds two or more empty lines.

- `^.{101,}$` finds rows that contain more than 100 characters.

- `\b(\w+)\s*=\s*\1\b` finds `w = w` where `w` is some arbitrary word.

- `\b(\w+)\s*=\s*(\w+)\s*(?:,\s*\b(\w+)\s*=\s*(\w+)\s*){1,}` finds comma separated assignments like
  `Foo with a = b, c = d` (we prefer using `;` like in `Foo with a = b; c = d`).

- `submitMulti \[\w+\] \[\]` finds `submitMulti` which can be replaced by `submit`.

- `(\w+),(\w+)` finds comma separated values with no whitespace.
