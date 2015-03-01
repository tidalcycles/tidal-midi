module Sound.Tidal.VolcaKeys where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape { params = [
                            CC "portamento" 5,
                            CC "expression" 11,
                            CC "voice" 40,
                            CC "octave" 41,
                            CC "detune" 42,
                            CC "vcoegint" 43,
                            CC "kcutoff" 44,
                            CC "vcfegint" 45,
                            CC "lforate" 46,
                            CC "lfopitchint" 47,
                            CC "lfocutoffint" 48,
                            CC "attack" 49,
                            CC "decay" 50,
                            CC "sustain" 51,
                            CC "dtime" 52,
                            CC "dfeedback" 53
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
