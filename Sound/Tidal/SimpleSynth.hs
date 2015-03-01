module Sound.Tidal.SimpleSynth where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape {params = [
                          CC "kcutoff" 69,
                          CC "portamento" 5,
                          CC "expression" 11,
                          CC "voice" 31,
                          CC "octave" 27,
                          CC "detune" 29,
                          CC "vcoegint" 43,
                          CC "vcfegint" 45,
                          CC "lforate" 16,
                          CC "lfopitchint" 47,
                          CC "lfocutoffint" 48,
                          CC "osc1vol" 52,
                          CC "osc1pan" 53,
                          CC "ringmod" 54,
                          CC "ringpan" 55,
                          CC "noise" 60,
                          CC "noisepan" 61,
                          CC "noisecol" 62,
                          CC "attack" 101,
                          CC "decay" 102,
                          CC "sustain" 103
                        ],
                        duration = ("dur", 0.05),
                        latency = 0.1}

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
osc1vol = makeF oscKeys "osc1vol"
osc1pan = makeF oscKeys "osc1pan"
ringmod = makeF oscKeys "ringmod"
ringpan = makeF oscKeys "ringpan"
noise = makeF oscKeys "noise"
noisepan = makeF oscKeys "noisepan"
noisecol = makeF oscKeys "noisecol"
