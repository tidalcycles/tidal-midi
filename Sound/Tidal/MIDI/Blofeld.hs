module Sound.Tidal.Blofeld where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape {params = [
                          mCC "portamento" 5,
                          mCC "expression" 11,
                          CC "lfoshape" 15 (0, 5) 0 passThru, -- 0..5 - sine,triangle,square,saw,random,sample&hold
                          mCC "lforate" 16,
                          CC "lfosync" 17 (0, 1) 0 passThru, -- 0 off, 1 on
                          mCC "lfodelay" 18,
                          CC "octave" 27 (16, 112) 0 passThru, -- 16, 28, 40 .. 112 - 128' .. 1/2'
                          CC "semitone" 28 (52, 76) 0.5 passThru, -- 52 .. 76 - -12 - +12 semitones
                          mCC "detune" 29,
                          mCC "osc1fm" 30,
                          SysEx "osc1fmsrc" 6 (0, 11) 0 passThru,
                          CC "osc1shape" 31 (0, 5) 0 passThru, -- 0..5 - pulse, saw, tri, sine, alt 1, alt 2
                          mCC "osc1pw" 33,
                          mCC "osc1pwm" 34,
                          SysEx "osc1pwmsrc" 10 (0, 30) 0 passThru,
                          mCC "osc1vol" 52,
                          mCC "osc1pan" 53,
                          mCC "ringmod" 54,
                          mCC "ringpan" 55,
                          mCC "noise" 60,
                          mCC "noisepan" 61,
                          mCC "noisecol" 62,
                          mCC "kcutoff" 69,
                          mCC "attack" 101,
                          mCC "decay" 102,
                          mCC "sustain" 103,
                          mCC "release" 106
                        ],
                        duration = ("dur", 0.05),
                        velocity = ("vel", 0.5),
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
osc1fmsrc = makeF oscKeys "osc1fmsrc"
osc1shape = makeF oscKeys "osc1shape"
osc1pw = makeF oscKeys "osc1pw"
osc1pwm = makeF oscKeys "osc1pwm"
osc1pwmsrc = makeF oscKeys "osc1pwmsrc"
osc1vol = makeF oscKeys "osc1vol"
osc1pan = makeF oscKeys "osc1pan"

ringmod = makeF oscKeys "ringmod"
ringpan = makeF oscKeys "ringpan"

noise = makeF oscKeys "noise"
noisepan = makeF oscKeys "noisepan"
noisecol = makeF oscKeys "noisecol"
