module Sound.Tidal.VolcaKeys where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape { params = [
                            mCC "portamento" 5,
                            mCC "expression" 11,
                            mCC "voice" 40,
                            mCC "octave" 41,
                            mCC "detune" 42,
                            mCC "vcoegint" 43,
                            mCC "kcutoff" 44,
                            mCC "vcfegint" 45,
                            mCC "lforate" 46,
                            mCC "lfopitchint" 47,
                            mCC "lfocutoffint" 48,
                            mCC "attack" 49,
                            mCC "decay" 50,
                            mCC "sustain" 51,
                            mCC "dtime" 52,
                            mCC "dfeedback" 53
                          ],
                         duration = ("dur", 0.05),
                         velocity = ("vel", 0.5),
                         latency = 0.1
                       }

oscKeys = toOscShape keys

note = makeI oscKeys "note"
dur  = makeF oscKeys "dur"
por  = makeF oscKeys "portamento"
expr = makeF oscKeys "expression"
oct  = makeF oscKeys "octave"
voi  = makeF oscKeys "voice"
det  = makeF oscKeys "detune"
vco  = makeF oscKeys "vcoegint"
ctf  = makeF oscKeys "kcutoff"
vcf  = makeF oscKeys "vcfegint"
lfo  = makeF oscKeys "lforate"
lfop = makeF oscKeys "lfopitchint"
lfoc = makeF oscKeys "lfocutoffint"
att  = makeF oscKeys "attack"
dec  = makeF oscKeys "decay"
sus  = makeF oscKeys "sustain"
dt   = makeF oscKeys "dtime"
df   = makeF oscKeys "dfeedback"
