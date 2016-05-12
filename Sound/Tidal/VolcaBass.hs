module Sound.Tidal.VolcaBass where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

bass :: ControllerShape
bass = ControllerShape { params = [
                            mCC "slide" 5,
                            mCC "expr" 11,
                            mCC "oct" 40,
                            mCC "lfo" 41,
                            mCC "lfoi" 42,
                            mCC "pit1" 43,
                            mCC "pit2" 44,
                            mCC "pit3" 45,
                            mCC "att" 46,
                            mCC "dec" 47,
                            mCC "ctf" 48,
                            mCC "gate" 49
                          ],
                         duration = ("dur", 0.05),
                         velocity = ("vel", 0.5),
                         latency = 0.1
                       }

oscBass = toOscShape bass

note         = makeI oscBass "note"
dur          = makeF oscBass "dur"
slide        = makeF oscBass "slide"
expr         = makeF oscBass "expr"
oct          = makeF oscBass "oct"
lfo          = makeF oscBass "lfo"
lfoi         = makeF oscBass "lfoi"
pit1         = makeF oscBass "pit1"
pit2         = makeF oscBass "pit2"
pit3         = makeF oscBass "pit3"
att          = makeF oscBass "att"
dec          = makeF oscBass "dec"
ctf          = makeF oscBass "ctf"
gate         = makeF oscBass "gate"
