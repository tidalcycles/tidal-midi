module Sound.Tidal.Synthino where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape { params = [
                            mCC "attack" 73,
                            mCC "decay" 75,
                            mCC "sustain" 79,
                            mCC "release" 72,
                            mCC "waveform" 70,
                            mCC "pitchlforate" 76,
                            mCC "pitchlfodepth" 1,
                            mCC "lfowaveform" 12,
                            mCC "filterlforate" 13,
                            mCC "filterlfodepth" 91,
                            mCC "peak" 71,
                            mCC "kcutoff" 74,
                            mCC "bpm" 16,
                            mCC "arplength" 17,
                            mCC "arptranspose" 18,
                            mCC "vol" 7,
                            mCC "off" 123
                          ],
                         duration = ("dur", 0.05),
                         velocity = ("vel", 0.5),
                         latency = 0.1
                       }

oscKeys = toOscShape keys

note           = makeI oscKeys "note"
dur            = makeF oscKeys "dur"
attack         = makeF oscKeys "attack"
decay          = makeF oscKeys "decay"
sustain        = makeF oscKeys "sustain"
release        = makeF oscKeys "release"
waveform       = makeF oscKeys "waveform"
pitchlforate   = makeF oscKeys "pitchlforate"
pitchlfodepth  = makeF oscKeys "pitchlfodepth"
lfowaveform    = makeF oscKeys "lfowaveform"
filterlforate  = makeF oscKeys "filterlforate"
filterlfodepth = makeF oscKeys "filterlfodepth"
peak           = makeF oscKeys "peak"
kcutoff        = makeF oscKeys "kcutoff"
bpm            = makeF oscKeys "bpm"
arplength     = makeF oscKeys "arplength"
arptranspose      = makeF oscKeys "arptranspose"
vol            = makeF oscKeys "vol"
off            = makeF oscKeys "off"
