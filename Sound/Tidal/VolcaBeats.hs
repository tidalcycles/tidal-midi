module Sound.Tidal.VolcaBeats where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control
import Control.Applicative

beats :: ControllerShape
beats = ControllerShape { params = [
                            mCC "lKick" 40,
                            mCC "lSnare" 41,
                            mCC "lLoTom" 42,
                            mCC "lHiTom" 43,
                            mCC "lClHat" 44,
                            mCC "lOpHat" 45,
                            mCC "lClap" 46,
                            mCC "lClaves" 47,
                            mCC "lAgogo" 48,
                            mCC "lCrash" 49,
                            mCC "sClap" 50,
                            mCC "sClaves" 51,
                            mCC "sAgogo" 52,
                            mCC "sCrash" 53,
                            mCC "stutterTime" 54,
                            mCC "stutterDepth" 55,
                            mCC "tomDecay" 56,
                            mCC "clHatDecay" 57,
                            mCC "opHatDecay" 58,
                            mCC "hatGrain" 59
                          ],
                         duration = ("dur", 0.05),
                         velocity = ("vel", 0.5),
                         latency = 0.1
                       }

oscBeats = toOscShape beats

drum = (makeI oscBeats "note") . (noteN <$>)

lbd = makeF oscBeats "lKick"
lsn = makeF oscBeats "lSnare"
llt = makeF oscBeats "lLoTom"
lht = makeF oscBeats "lHiTom"
lch = makeF oscBeats "lClHat"
loh = makeF oscBeats "lOpHat"
lcp = makeF oscBeats "lClap"
lcl = makeF oscBeats "lClaves"
lag = makeF oscBeats "lAgogo"
lcr = makeF oscBeats "lCrash"
scp = makeF oscBeats "sClap"
scl = makeF oscBeats "sClaves"
sag = makeF oscBeats "sAgogo"
scr = makeF oscBeats "sCrash"
stt = makeF oscBeats "stutterTime"
std = makeF oscBeats "stutterDepth"
dt = makeF oscBeats "tomDecay"
dch = makeF oscBeats "clHatDecay"
doh = makeF oscBeats "opHatDecay"
dhg = makeF oscBeats "hatGrain"

noteN :: String -> Int
noteN "bd"  = 36
noteN "sn"  = 38
noteN "lt"  = 43
noteN "ht"  = 50
noteN "ch"  = 42
noteN "oh"  = 46
noteN "cp"  = 39
noteN "cl"  = 75
noteN "ag"  = 67
noteN "cr"  = 49
noteN _ = 0
