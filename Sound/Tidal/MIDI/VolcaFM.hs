module Sound.Tidal.MIDI.VolcaFM where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

fmController :: ControllerShape
fmController = ControllerShape { controls = [
                                      mCC octave_p 40,
                                      mCC velocity_p 41,
                                      mCC modattack_p 42,
                                      mCC moddecay_p 43,
                                      mCC carattack_p 44,
                                      mCC cardecay_p 45,
                                      mCC lfo_p 46,
                                      mCC lfopitchint_p 47,
                                      mCC algtm_p 48,
                                      mCC arp_p 49,
                                      mCC arpdiv_p 50
                                      ],
                                   --duration = ("dur", 0.05),
                                   --velocity = ("vel", 0.5),
                                   latency = 0.01
                                 }

(modattack, modattack_p) = pF "modattack" (Just 0)
(moddecay, moddecay_p) = pF "moddecay" (Just 0)
(carattack, carattack_p) = pF "carattack" (Just 0)
(cardecay, cardecay_p) = pF "cardecay" (Just 0)
(algtm, algtm_p) = pF "algtm" (Just 0)
(arp, arp_p) = pF "arp" (Just 0)
(arpdiv, arpdiv_p) = pF "arpdiv" (Just 0)

fm = toShape fmController
