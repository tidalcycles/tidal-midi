module Sound.Tidal.VolcaKeys where

import Sound.Tidal.Context hiding (latency)
import Sound.Tidal.MIDI.Control


keys :: ControllerShape
keys = ControllerShape {
  controls = [
     mCC portamento_p 5,
     mCC expression_p 11,
     mCC voice_p 40,
     mCC octave_p 41,
     mCC detune_p 42,
     mCC vcoegint_p 43,
     mCC cutoff_p 44,
     mCC vcfegint_p 45,
     mCC lforate_p 46,
     mCC lfopitchint_p 47,
     mCC lfocutoffint_p 48,
     mCC attack_p 49,
     mCC decay_p 50,
     mCC sustain_p 51,
     mCC delaytime_p 52,
     mCC delayfeedback_p 53
     ],
  latency = 0.01
  }



-- general shape for stream
keysShape = toShape keys

-- params
