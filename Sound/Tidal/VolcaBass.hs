module Sound.Tidal.VolcaBass where

import Sound.Tidal.Context hiding (latency)
import Sound.Tidal.MIDI.Control

bass :: ControllerShape
bass = ControllerShape {
  controls = [
     mCC slide_p 5,
     mCC expression_p 11,
     mCC octave_p 40,
     mCC lforate_p 41,
     mCC lfoint_p 42,
     mCC pitch1_p 43,
     mCC pitch2_p 44,
     mCC pitch3_p 45,
     mCC attack_p 46,
     mCC decay_p 47,
     mCC cutoffegint_p 48,
     mCC gate_p 49
     ],
  latency = 0.1
  }


-- general shape for stream
bassShape = toShape bass
