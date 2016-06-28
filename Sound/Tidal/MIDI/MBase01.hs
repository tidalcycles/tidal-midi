module Sound.Tidal.MIDI.MBase01 where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

mbase01Controller :: ControllerShape
mbase01Controller = ControllerShape { controls = [
                          mCC tune_p 100,
                          mCC pitch_p 101,
                          mCC decay_p 102,
                          mCC harmonics_p 103,
                          mCC pulse_p 104,
                          mCC noise_p' 105,
                          mCC attack_p 106,
                          mCC eqlzr_p 107
                        ],
                        --duration = ("dur", 0.05),
                        --velocity = ("vel", 0.5),
                        latency = 0.1}

mbase01 = toShape mbase01Controller

(tune, tune_p) = pF "tune" (Just 0)
(pitch, pitch_p) = pF "pitch" (Just 0)
(harmonics, harmonics_p) = pF "harmonics" (Just 0)
(pulse, pulse_p) = pF "pulse" (Just 0)
(_, noise_p') = pF "noise" (Just 0)
(eqlzr, eqlzr_p) = pF "eqlzr" (Just 0)
