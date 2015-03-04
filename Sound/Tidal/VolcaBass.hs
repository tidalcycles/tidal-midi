module Sound.Tidal.VolcaBass where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

bass :: ControllerShape
bass = ControllerShape { params = [
                            mCC "slide" 5,
                            mCC "expression" 11,
                            mCC "octave" 40,
                            mCC "lforate" 41,
                            mCC "lfoint" 42,
                            mCC "pitch1" 43,
                            mCC "pitch2" 44,
                            mCC "pitch3" 45,
                            mCC "attack" 46,
                            mCC "decay" 47,
                            mCC "cutoff" 48,
                            mCC "gate" 49
                          ],
                         duration = ("dur", 0.05),
                         latency = 0.1
                       }

oscBass = toOscShape bass

note         = makeI oscBass "note"
dur          = makeF oscBass "dur"
slide        = makeF oscBass "slide"
expression   = makeF oscBass "expression"
octave       = makeF oscBass "octave"
lforate      = makeF oscBass "lforate"
lfoint       = makeF oscBass "lfoint"
pitch1       = makeF oscBass "pitch1"
pitch2       = makeF oscBass "pitch2"
pitch3       = makeF oscBass "pitch3"
attack       = makeF oscBass "attack"
decay        = makeF oscBass "decay"
cutoff       = makeF oscBass "cutoff"
gate         = makeF oscBass "gate"
