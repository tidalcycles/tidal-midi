module Sound.Tidal.VolcaBass where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

bass :: ControllerShape
bass = ControllerShape { params = [
                            CC "slide" 5,
                            CC "expression" 11,
                            CC "octave" 40,
                            CC "lforate" 41,
                            CC "lfoint" 42,
                            CC "pitch1" 43,
                            CC "pitch2" 44,
                            CC "pitch3" 45,
                            CC "attack" 46,
                            CC "decay" 47,
                            CC "cutoff" 48,
                            CC "gate" 49
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
