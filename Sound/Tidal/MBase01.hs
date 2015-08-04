module Sound.Tidal.MBase01 where
import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

mbase01 :: ControllerShape
mbase01 = ControllerShape {params = [
                          mCC "tune" 100,
                          mCC "pitch" 101,
                          mCC "decay" 102,
                          mCC "harmonics" 103,
                          mCC "pulse" 104,
                          mCC "noise" 105,
                          mCC "attack" 106,
                          mCC "eqlzr" 107
                        ],
                        duration = ("dur", 0.05),
                        velocity = ("vel", 0.5),
                        latency = 0.1}

oscKeys = toOscShape mbase01

note         = makeI oscKeys "note"
dur          = makeF oscKeys "dur"


tune = makeF oscKeys "tune"
pitch = makeF oscKeys "pitch"
decay = makeF oscKeys "decay"
harmonics = makeF oscKeys "harmonics"
pulse = makeF oscKeys "pulse"
noise = makeF oscKeys "noise"
attack = makeF oscKeys "attack"
eqlzr = makeF oscKeys "eqlzr"
