module Sound.Tidal.SimpleSynth where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape {params = [
                          mCC "modwheel" 1,
                          mCC "balance" 10,
                          mCC "expression" 11,
                          mCC "sustainpedal" 64
                        ],
                        duration = ("dur", 0.05),
                        velocity = ("vel", 0.5),
                        latency = 0.1}

oscKeys = toOscShape keys

note         = makeI oscKeys "note"
dur          = makeF oscKeys "dur"
vel          = makeF oscKeys "vel"
modwheel     = makeF oscKeys "modwheel"
balance          = makeF oscKeys "balance"
expression   = makeF oscKeys "expression"
sustainpedal = makeF oscKeys "sustainpedal"
