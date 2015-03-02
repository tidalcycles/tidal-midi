module Sound.Tidal.Tetra where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

import Sound.Tidal.Parse

polysynth :: ControllerShape
polysynth = ControllerShape { params = [
                                  NRPN "kcutoff" 15 (0, 164),
                                  NRPN "kresonance" 16 (0, 127),

                                  -- filter envelope
                                  NRPN "atk" 23 (0, 127),
                                  NRPN "dcy" 24 (0, 127),
                                  NRPN "sus" 25 (0, 127),
                                  NRPN "rel" 26 (0, 127),

                                  NRPN "fgain" 110 (0, 127),
                                  NRPN "fvol" 116 (0, 127),

                                  NRPN "audiomod" 18 (0, 127),
                                  NRPN "kamt" 17 (0, 127),

                                  NRPN "oscmix" 13 (0, 127),
                                  NRPN "sub1vol" 114 (0, 127),
                                  NRPN "sub2vol" 115 (0, 127),
                                  NRPN "noise" 14 (0, 127),

                                  NRPN "fpoles" 19 (0, 1),

                                  NRPN "kmode" 119 (0, 2),

                                  NRPN "ksplitpoint" 118 (0, 127)
                          ],
                         duration = ("dur", 0.05),
                         latency = 0.1
                       }

oscPolysynth = toOscShape polysynth

note            = makeI oscPolysynth "note"

dur             = makeF oscPolysynth "dur"

kcutoff         = makeF oscPolysynth "kcutoff"
kresonance      = makeF oscPolysynth "kresonance"
atk             = makeF oscPolysynth "atk"
dcy             = makeF oscPolysynth "dcy"
sus             = makeF oscPolysynth "sus"
rel             = makeF oscPolysynth "rel"
fgain           = makeF oscPolysynth "fgain"
fvol            = makeF oscPolysynth "fvol"

audiomod        = makeF oscPolysynth "audiomod"
kamt            = makeF oscPolysynth "kamt"

oscmix          = makeF oscPolysynth "oscmix"
sub1vol         = makeF oscPolysynth "sub1vol"
sub2vol         = makeF oscPolysynth "sub2vol"
noise           = makeF oscPolysynth "noise"


fpoles          = makeF oscPolysynth "fpoles"
twopole         = fpoles (p "0")
fourpole        = fpoles (p "1")

kmode           = makeF oscPolysynth "kmode"
knormal         = kmode (p "0")
kstack          = kmode (p "1")
ksplit          = kmode (p "2")

ksplitpoint     = makeF oscPolysynth "ksplitpoint"
