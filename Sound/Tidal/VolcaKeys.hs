module Sound.Tidal.VolcaKeys where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape { params = [
                            mCC "por" 5,
                            mCC "expr" 11,
                            mCC "voi" 40,
                            mCC "oct" 41,
                            mCC "det" 42,
                            mCC "vco" 43,
                            mCC "ctf" 44,
                            mCC "vcf" 45,
                            mCC "lfo" 46,
                            mCC "lfop" 47,
                            mCC "lfoc" 48,
                            mCC "att" 49,
                            mCC "dec" 50,
                            mCC "sus" 51,
                            mCC "dt" 52,
                            mCC "df" 53
                          ],
                         duration = ("dur", 0.05),
                         velocity = ("vel", 0.5),
                         latency = 0.1
                       }

oscKeys = toOscShape keys

note = makeI oscKeys "note"
dur  = makeF oscKeys "dur"
por  = makeF oscKeys "por"
expr = makeF oscKeys "expr"
oct  = makeF oscKeys "oct"
voi  = makeF oscKeys "voi"
det  = makeF oscKeys "det"
vco  = makeF oscKeys "vco"
ctf  = makeF oscKeys "ctf"
vcf  = makeF oscKeys "vcf"
lfo  = makeF oscKeys "lfo"
lfop = makeF oscKeys "lfop"
lfoc = makeF oscKeys "lfoc"
att  = makeF oscKeys "att"
dec  = makeF oscKeys "dec"
sus  = makeF oscKeys "sus"
dt   = makeF oscKeys "dt"
df   = makeF oscKeys "df"
