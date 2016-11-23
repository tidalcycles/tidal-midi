module Sound.Tidal.SimpleSynth where


import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control


keysController :: ControllerShape
keysController = ControllerShape { controls = [
							mCC modwheel_p 1,
		                    mCC balance_p 10,
		                    mCC expression_p 11,
		                    mCC sustainpedal_p 64						
                          ],
                         latency = 0.01
                       }

keys = toShape keysController

(modwheel,	   modwheel_p)     = pF "modwheel"     (Just 0)
(balance,	   balance_p)      = pF "balance"      (Just 0)
(expression,   expression_p)   = pF "expression"   (Just 0)
(sustainpedal, sustainpedal_p) = pF "sustainpedal" (Just 0)

