# safe-tidal-cli

This is for running tidal-as-a-service (for collaborative editors)
in a somewhat safer way.

See https://github.com/tidalcycles/Tidal/issues/627 

This package contains a library (`safe-tidal`) and an executable (`safe-tidal-cli`).

Install
```
cabal install tidal safe-tidal --lib # installs libraries
cabal install safe-tidal  --constraint 'tidal==1.4.9' # installs executable
```

The executable will connect to SuperDirt on startup.

Then it accepts text blocks on stdin  (`d1 $ s "bd"` etc.)
where a blocks are separated by empty lines.

These expressions appear to be of type `IO ()`
but actually it's a safe wrapper
that only allows Tidal's top-level operators.
(Currently, not even all of them.)

You can write any expression of that type, e.g., 
```
let x = .. in do { d1 $ ... ; d2 $ ... }

hush
```

