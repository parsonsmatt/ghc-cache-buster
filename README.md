# ghc-cache-buster

This project is an attempt to document GHC's incremental compilation behavior.
This information should guide module organization.  Specifically, we want to
study the effect of `TemplateHaskell` and re-exports to identify what terms
need to be imported separately.

# Scenarios

This module structure is setup to mimic a "real world" application.  There's a
`GBC.Types` module which re-exports some type definitions and common functions.
A module `GBC.Types.Quuz` has a `TemplateHaskell` `QuasiQuoter` defined, and TH
functions can break caches.  So let's figure out what causes a recompilation to
happen.

## Clean Build

```
Building library for ghc-cache-buster-0.1.0.0..
[ 1 of 10] Compiling GCB.Prelude
[ 2 of 10] Compiling GCB.Types.Bar
[ 3 of 10] Compiling GCB.Types.Foo
[ 4 of 10] Compiling GCB.Types.FooExplicit
[ 5 of 10] Compiling GCB.Types.Quuz
[ 6 of 10] Compiling GCB.Types
[ 7 of 10] Compiling GCB.App
[ 8 of 10] Compiling GCB.Explicit
[ 9 of 10] Compiling GCB.Web
[10 of 10] Compiling Paths_ghc_cache_buster
```

These are all of the modules in the project currently.
To run these test, we'll make a change to a module, and see what ends up getting recompiled.

`GCB.Prelude` exports things from outside the project.
`GCB.Types.Bar` is not  re-exported from `GCB.Types`, but the other modules in the namespace are.
`GCB.Types.Quuz` defines some Template Haskell functions.
`GBC.Types` re-exports things, as well as defining a type in it's own module.

`GCB.App` does an open import of `GCB.Types` and uses functions from `GCB.Types.Foo` and `GCB.Types.Quuz`

## `GCB.Types.Foo`

This module has an open export.  Adding and removing items from the module
should change the interface of the module, which should trigger recompilation
of dependent modules.  The module is re-exported from `GCB.Types`.

Uncommenting the `addingThing` line causes this recompilation behavior:

```
[ 6 of 10] Compiling GCB.Types.Foo
[ 7 of 10] Compiling GCB.Types [GCB.Types.Foo changed]
[ 8 of 10] Compiling GCB.App [GCB.Types changed]
[ 9 of 10] Compiling GCB.Explicit [GCB.Types.Foo changed]
[10 of 10] Compiling GCB.Web [GCB.Types changed]
```

- `GCB.Types` re-exports `GCB.Types.Foo` in entirety, so the export signature changed.
  This triggers a re-compilation of everything that depends on `GCB.Types`.
- `GCB.App` depends on `GCB.Types`, and it uses `mkFoo` from an open import of 
  `GCB.Types`.
- `GCB.Explicit` depends on `GCB.Types.Foo`, but explicitly imports `mkFoo`. So
  an explicit import list is not enough to defeat recompilation here. GHC could
  potentially check what *changed* in the interface files and only trigger
  recompilation if the explicitly used part of the module has changed. This
  could possibly happen even with an implicit import list, but that would be
  much more work than just parsing the module signature.
- `GCB.Web` has an empty import of `GCB.Types`, and yet still is recompiled.

## `GCB.Types.FooExplicit`

This modules contains an explicit export list.
It is otherwise identical to `Foo` in structure, but with terms adding a
`FooExplicit` tag to differentiate them and prevent name conflicts.

`FooExplicit` is re-exported by `GCB.Types`.
Nothing depends on `FooExplicit`, at all, so ideally, modifying the file should not cause *anything* to recompile.
Certainly, nothing should be recompiled if the export list doesn't change.

Let's uncomment addingThing from here and see what happens.

```
Building library for ghc-cache-buster-0.1.0.0..
[ 6 of 10] Compiling GCB.Types.FooExplicit
[ 8 of 10] Compiling GCB.App [TH]
[ 9 of 10] Compiling GCB.Explicit [TH]
```

The public signature did not change.
Yet we still recompiled `GCB.App` and `GCB.Explicit`, with the reason given as `[TH]`. 
Why did that happen?

As of this writing, the import of `GCB.App` looks like this:

```haskell
module GCB.App where

import GCB.Prelude

import GCB.Types

app :: IO ()
app = do
    putStrLn "Hello, World!"
    let x = [compileQuuz|asdf|]
        foo = mkFoo "asdf"
    putStrLn "Goodbye, World!"

blargh
```

We use two `TemplateHaskell` terms, both of which come from `GCB.Types` and are originally defined in `GCB.Types.Quuz`.
The module interface of `GCB.Types` did not change, but we still triggered a recompilation.
This is suboptimal, but understandable.

What about `GCB.Explicit`? Here's that module:

```haskell
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module GCB.Explicit where

import GCB.Types.Foo (mkFoo)
import GCB.Types.FooExplicit (mkFooExplicit)
import GCB.Types.Quuz (compileQuuz, blargh)

app :: IO ()
app = do
    putStrLn "Hello, World!"
    let x = [compileQuuz|asdf|]
        foo = mkFoo "asdf"
    putStrLn "Goodbye, World!"

blargh
```

We altered `FooExplicit` in a way that *did not* modify the signature.
Yet we still recompiled the entire module, with the reason being `TH`.
The `TH` is coming from `GCB.Types.Quuz`.
What's going on here?
This feels like a bug in GHC.

Let's see if we can isolate which of the two features causes it - the top level splice `blargh` or the quasiquote `compileQuuz`.
Commenting out `blargh` and modifying `FooExplicit` causes the same recompilation behavior.
Putting `blargh` back in, commenting out the `let x = [compileQuuz|asdf|]`, and modifying `FooExplicit` causes *the same recompilation behavior*.
What exactly is causing this?
