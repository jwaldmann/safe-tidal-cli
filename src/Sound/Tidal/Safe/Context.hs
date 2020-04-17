-- | wraps IO functions

{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}

module Sound.Tidal.Safe.Context
  ( Op () -- do not export constructor,
    -- so the user has no way of putting arbitraty IO stuff
    -- in "Op", and below "run"
  , exec
  , streamReplace
  , streamHush
  , streamList
  , streamMute
  , streamUnmute
  , streamSolo
  , streamUnsolo
  , streamOnce
  , streamFirst
  , streamNudgeAll
  , streamAll
  , streamResetCycles
  , streamSetI
  , streamSetF
  , streamSetS
  , streamSetR
  , streamSetB
  , transition
  , module C
  , startTidal, superdirtTarget, OSCTarget(..)
  )
where

import Data.Ratio as C
import Sound.Tidal.Carabiner as C
import Sound.Tidal.Config as C
import Sound.Tidal.Control as C
import Sound.Tidal.Core as C
import Sound.Tidal.Params as C
import Sound.Tidal.ParseBP as C
import Sound.Tidal.Pattern as C
import Sound.Tidal.Scales as C
import Sound.Tidal.Simple as C
import Sound.Tidal.Stream
  (startTidal, OSCTarget(..), superdirtTarget)
-- import Sound.Tidal.Transition as C
import Sound.Tidal.UI as C
import Sound.Tidal.Version as C
import Sound.Tidal.EspGrid as C

import qualified Sound.Tidal.Context as C
import Sound.Tidal.Context
  (Stream, Pattern, ControlPattern, Time)
import Control.Monad.Reader

newtype Op r = Op ( ReaderT Stream IO r )
  deriving (Functor, Applicative, Monad)

exec :: Stream -> Op r -> IO r
exec s (Op m) = runReaderT m s

op1 f         = Op $ do a <- ask; lift $ f a
op2 f b       = Op $ do a <- ask; lift $ f a b 
op3 f b c     = Op $ do a <- ask; lift $ f a b c
op4 f b c d   = Op $ do a <- ask; lift $ f a b c d
op5 f b c d e = Op $ do a <- ask; lift $ f a b c d e

streamReplace = op3 C.streamReplace
streamHush = op1 C.streamHush
streamList = op1 C.streamList
streamMute = op2 C.streamMute
streamUnmute = op2 C.streamUnmute
streamSolo = op2 C.streamSolo
streamUnsolo = op2 C.streamUnsolo
streamOnce = op2 C.streamOnce
streamFirst = op2 C.streamFirst
streamNudgeAll = op2 C.streamNudgeAll
streamAll = op2 C.streamAll
streamResetCycles = op1 C.streamResetCycles
transition = op5 C.transition
streamSetI = op3 C.streamSetI
streamSetF = op3 C.streamSetF
streamSetS = op3 C.streamSetS
streamSetR = op3 C.streamSetR
streamSetB = op3 C.streamSetB
