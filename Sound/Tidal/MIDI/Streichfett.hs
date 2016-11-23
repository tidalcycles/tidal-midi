--This module is based on the SimpleSynth module
--Based on the tutorial at doc/synth-mapping.md

module Sound.Tidal.MIDI.Streichfett where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

fettController :: ControllerShape
fettController = ControllerShape { controls = [
							mCC phaser_p 93							
                          ],
                         latency = 0.01
                       }

fett = toShape fettController

(phaser,  phaser_p) = pF "phaser" (Just 0)
