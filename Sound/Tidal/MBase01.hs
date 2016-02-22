module Sound.Tidal.MBase01 where

import Sound.Tidal.Context hiding (latency)
import Sound.Tidal.MIDI.Control

mbase01 :: ControllerShape
mbase01 = ControllerShape {
  controls = [
     mCC detune_p 100,
     mCC pitch1_p 101,
     mCC decay_p 102,
     mCC vcoegint_p 103,
     mCC vcfegint_p 104,
     mCC shape_p 105,
     mCC attack_p 106,
     mCC voice_p 107
     ],
  latency = 0.1
  }

mbase01Shape = toShape mbase01
