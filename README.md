# safe-tidal-cli

See https://github.com/tidalcycles/Tidal/issues/627

* connects to SuperDirt on startup
* accepts text blocks of the shape "dX $ ..." on stdin
* evaluates the argument with https://hackage.haskell.org/package/mueval , applies "dX" to the result
* prints something useful to stdout and stderr

