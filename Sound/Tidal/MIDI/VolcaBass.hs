module Sound.Tidal.VolcaBass where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

bassController :: ControllerShape
bassController = ControllerShape { controls = [
                                      mCC slide_p 5,
                                      mCC expression_p 11,
                                      mCC octave_p 40,
                                      mCC lfo_p 41,
                                      mCC lfoint_p 42,
                                      mCC pitch1_p 43,
                                      mCC pitch2_p 44,
                                      mCC pitch3_p 45,
                                      mCC attack_p 46,
                                      mCC decay_p 47,
                                      mCC cutoff_p 48,
                                      mCC gate_p 49
                                      ],
                                   --duration = ("dur", 0.05),
                                   --velocity = ("vel", 0.5),
                                   latency = 0.01
                                 }
                 
bass = toShape bassController

