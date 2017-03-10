module Sound.Tidal.MIDI.Synth where

import Sound.Tidal.MIDI.Control
import Sound.Tidal.Params

synthController :: ControllerShape
synthController =
  ControllerShape
  { controls =
      [ mCC modwheel_p 1
      , mCC pan_p 10
      , mCC expression_p 11
      , mCC sustainpedal_p 64
      ]
  , latency = 0.1
  }

synth = toShape synthController
