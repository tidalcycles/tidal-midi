module Sound.Tidal.VolcaKeys where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

keysController :: ControllerShape
keysController = ControllerShape { controls = [
                            mCC portamento_p 5,
                            mCC expression_p 11,
                            mCC voice_p 40,        -- voi
                            mCC octave_p 41,       -- oct
                            mCC detune_p 42,       -- det
                            mCC vcoegint_p 43,     -- vco
                            mCC cutoff_p 44,       -- ctf
                            mCC vcfegint_p 45,     -- vcf
                            mCC lfo_p 46,
                            mCC lfopitchint_p 47,  -- lfop
                            mCC lfocutoffint_p 48, -- lfoc
                            mCC attack_p 49,       -- att
                            mCC decay_p 50,        -- dec
                            mCC sustain_p 51,      -- sus
                            mCC delaytime_p 52,    -- delayt
                            mCC delayfeedback_p 53 -- delayfb
                          ],
                          -- duration = ("dur", 0.05),
                          -- velocity = ("vel", 0.5),
                         latency = 0.01
                       }

keys = toShape keysController
