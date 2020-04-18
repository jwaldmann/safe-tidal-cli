# safe-tidal-cli

This is for running
"[tidal](https://tidalcycles.org/)-as-a-service"
(for collaborative editors) in a somewhat safer way.

See https://github.com/tidalcycles/Tidal/issues/627 

This package contains a library (`safe-tidal`)
and an executable (`safe-tidal-cli`).

The indented use case is with https://github.com/munshkr/flok :
```
node packages/repl/bin/flok-repl.js -H ws://localhost:3000 -s <token> -n tidal -- safe-tidal-cli
```

## Install
```
cabal install tidal safe-tidal --lib # installs libraries
cabal install safe-tidal  --constraint 'tidal==1.4.9' # installs executable
```

beware of cabal env-file hell,
cf. https://github.com/jwaldmann/safe-tidal-cli/issues/5

## Goals

The executable `safe-tidal-cli` aims to provide

* the essential subset of the functionality
   of a `ghci` session with `BootTidal` loaded,
* while being extra resilient to improper input.

`safe-tidal-cli` will connect to `SuperDirt` on startup.

Then it accepts text blocks on `stdin`  
where blocks are separated by empty lines.
In the intended use case, a block will be sent
by the `flok` server each time a client hits `Ctrl-Enter`.

Each block contains one expression, e.g., `d1 $ s "bd"`,
which `safe-tidal-cli` will evaluate (by the embedded `ghci`).
The resulting value is an action that gets executed
(by the embedded `tidal`). The execution involves
communication with the `SuperDirt`/`SuperCollider` back-end.

These expressions appear to be of type `IO ()`
but actually we have re-defined `d1` etc.
to use a safe wrapper
that only allows Tidal's top-level operators
(cf. https://github.com/jwaldmann/safe-tidal-cli/issues/8)
and probibits any other form of `IO`.

You can write any expression of that type, e.g.,
```
d1 $ s "bd"

let x = .. in do { d1 $ ... ; d2 $ ... }

hush
```

`safe-tidal-cli` will reject any expression of any other type,
in particular, of type `IO a`,
so no-one can execute `readFile "/etc/passwd"`.
It will also reject harmless expressions like `1+2`.
We may want to allow that.

`safe-tidal-cli` rejects all non-expressions.
That means you cannot interact with the state
of the `ghci` session. You cannot write

* imports (`import System.IO.Unsafe`),
* definitions (`let foo = bar`),
* queries (`:type`, `:info`, `:doc`).

We may lift the latter restriction.

`safe-tidal-cli` will exit when `stdin` is closed
and all blocks are processed.

## Related Work

The idea of using the GHC API (via `hint`) for `tidal`
is not new,
cf. https://github.com/tidalcycles/tidali/blob/master/src/Main.hs
The idea of wrapping `IO` to make it safe,
is also not new (TODO: references?).
Perhaps the combination is new.

