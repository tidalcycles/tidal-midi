module Sound.Tidal.SimpleSynth where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape {params = [
                          CC "modwheel" 1,
                          CC "balance" 10,
                          CC "expression" 11,
                          CC "sustainpedal" 64
                        ],
                        duration = ("dur", 0.05),
                        latency = 0.1}

oscKeys = toOscShape keys

note         = makeI oscKeys "note"
dur          = makeF oscKeys "dur"
modwheel     = makeF oscKeys "modwheel"
balance          = makeF oscKeys "balance"
expression   = makeF oscKeys "expression"
sustainpedal = makeF oscKeys "sustainpedal"
