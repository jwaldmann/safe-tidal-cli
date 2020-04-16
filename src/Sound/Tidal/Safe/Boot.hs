-- as in BootTidal but in the Op monad

{-# language NoMonomorphismRestriction #-}

module Sound.Tidal.Safe.Boot where

import Sound.Tidal.Safe.Context
import qualified Sound.Tidal.Transition

-- everyone is missing the tidal :: Stream argument
-- this will be provided by the Reader monad

p = streamReplace
hush = streamHush
list = streamList
mute = streamMute
unmute = streamUnmute
solo = streamSolo
unsolo = streamUnsolo
once = streamOnce
first = streamFirst
asap = once
nudgeAll = streamNudgeAll
all = streamAll
resetCycles = streamResetCycles
setcps = asap . cps
xfade i = transition True (Sound.Tidal.Transition.xfadeIn 4) i
xfadeIn i t = transition True (Sound.Tidal.Transition.xfadeIn t) i
histpan i t = transition True (Sound.Tidal.Transition.histpan t) i
wait i t = transition True (Sound.Tidal.Transition.wait t) i
waitT i f t = transition True (Sound.Tidal.Transition.waitT f t) i
jump i = transition True (Sound.Tidal.Transition.jump) i
jumpIn i t = transition True (Sound.Tidal.Transition.jumpIn t) i
jumpIn' i t = transition True (Sound.Tidal.Transition.jumpIn' t) i
jumpMod i t = transition True (Sound.Tidal.Transition.jumpMod t) i
mortal i lifespan release = transition True (Sound.Tidal.Transition.mortal lifespan release) i
interpolate i = transition True (Sound.Tidal.Transition.interpolate) i
interpolateIn i t = transition True (Sound.Tidal.Transition.interpolateIn t) i
clutch i = transition True (Sound.Tidal.Transition.clutch) i
clutchIn i t = transition True (Sound.Tidal.Transition.clutchIn t) i
anticipate i = transition True (Sound.Tidal.Transition.anticipate) i
anticipateIn i t = transition True (Sound.Tidal.Transition.anticipateIn t) i
forId i t = transition False (Sound.Tidal.Transition.mortalOverlay t) i

d1 = p 1 . (|< orbit 0)
d2 = p 2 . (|< orbit 1)
d3 = p 3 . (|< orbit 2)
d4 = p 4 . (|< orbit 3)
d5 = p 5 . (|< orbit 4)
d6 = p 6 . (|< orbit 5)
d7 = p 7 . (|< orbit 6)
d8 = p 8 . (|< orbit 7)
d9 = p 9 . (|< orbit 8)
d10 = p 10 . (|< orbit 9)
d11 = p 11 . (|< orbit 10)
d12 = p 12 . (|< orbit 11)
d13 = p 13
d14 = p 14
d15 = p 15
d16 = p 16

setI = streamSetI
setF = streamSetF
setS = streamSetS
setR = streamSetR
setB = streamSetB
