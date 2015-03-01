module Sound.Tidal.Blofeld where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape {params = [
                          CC "portamento" 5,
                          CC "expression" 11,
                          RPN "lfoshape" 15, -- 0..5 - sine,triangle,square,saw,random,sample&hold
                          CC "lforate" 16,
                          RPN "lfosync" 17, -- 0 off, 1 on
                          CC "lfodelay" 18,
                          RPN "octave" 27, -- 16, 28, 40 .. 112 - 128' .. 1/2'
                          RPN "semitone" 28, -- 52 .. 76 - -12 - +12 semitones
                          CC "detune" 29,
                          CC "osc1fm" 30,
                          RPN "osc1shape" 31, -- 0..5 - pulse, saw, tri, sine, alt 1, alt 2
                          CC "osc1pw" 33,
                          CC "osc1pwm" 34,
                          CC "osc1vol" 52,
                          CC "osc1pan" 53,
                          CC "ringmod" 54,
                          CC "ringpan" 55,
                          CC "noise" 60,
                          CC "noisepan" 61,
                          CC "noisecol" 62,
                          CC "kcutoff" 69,
                          CC "attack" 101,
                          CC "decay" 102,
                          CC "sustain" 103,
                          CC "release" 106
                        ],
                        duration = ("dur", 0.05),
                        latency = 0.1}

oscKeys = toOscShape keys

note         = makeI oscKeys "note"
dur          = makeF oscKeys "dur"

portamento   = makeF oscKeys "portamento"
expression   = makeF oscKeys "expression"
octave       = makeF oscKeys "octave"
semitone       = makeF oscKeys "semitone"
detune       = makeF oscKeys "detune"

kcutoff      = makeF oscKeys "kcutoff"

lforate      = makeF oscKeys "lforate"
lfoshape     = makeF oscKeys "lfoshape"
lfodelay     = makeF oscKeys "lfodelay"
lfosync     = makeF oscKeys "lfosyn"

attack       = makeF oscKeys "attack"
decay        = makeF oscKeys "decay"
sustain      = makeF oscKeys "sustain"
release      = makeF oscKeys "release"

osc1fm = makeF oscKeys "osc1fm"
osc1shape = makeF oscKeys "osc1shape"
osc1pw = makeF oscKeys "osc1pw"
osc1pwm = makeF oscKeys "osc1pwm"
osc1vol = makeF oscKeys "osc1vol"
osc1pan = makeF oscKeys "osc1pan"

ringmod = makeF oscKeys "ringmod"
ringpan = makeF oscKeys "ringpan"

noise = makeF oscKeys "noise"
noisepan = makeF oscKeys "noisepan"
noisecol = makeF oscKeys "noisecol"
