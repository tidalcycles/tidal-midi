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
                         latency = 0.1
                       }

oscKeys = toOscShape keys

note         = makeI oscKeys "note"
dur          = makeF oscKeys "dur"
portamento   = makeF oscKeys "portamento"
expression   = makeF oscKeys "expression"
octave       = makeF oscKeys "octave"
voice        = makeF oscKeys "voice"
detune       = makeF oscKeys "detune"
vcoegint     = makeF oscKeys "vcoegint"
kcutoff      = makeF oscKeys "kcutoff"
vcfegint     = makeF oscKeys "vcfegint"
lforate      = makeF oscKeys "lforate"
lfopitchint  = makeF oscKeys "lfopitchint"
lfocutoffint = makeF oscKeys "lfocutoffint"
attack       = makeF oscKeys "attack"
decay        = makeF oscKeys "decay"
sustain      = makeF oscKeys "sustain"
dtime        = makeF oscKeys "dtime"
dfeedback    = makeF oscKeys "dfeedback"
