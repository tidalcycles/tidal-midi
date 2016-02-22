module Sound.Tidal.VolcaBeats where

import Sound.Tidal.Context hiding (latency)
import Sound.Tidal.MIDI.Control

beats :: ControllerShape
beats = ControllerShape {
  controls = [
     mCC lkick_p 40,
     mCC lsnare_p 41,
     mCC llotom_p 42,
     mCC lhitom_p 43,
     mCC lclhat_p 44,
     mCC lophat_p 45,
     mCC lclap_p 46,
     mCC lclaves_p 47,
     mCC lagogo_p 48,
     mCC lcrash_p 49,
     mCC sclap_p 50,
     mCC sclaves_p 51,
     mCC sagogo_p 52,
     mCC scrash_p 53,
     mCC stuttertime_p 54,
     mCC stutterdepth_p 55,
     mCC tomdecay_p 56,
     mCC clhatdecay_p 57,
     mCC ophatdecay_p 58,
     mCC hatgrain_p 59
     ],
  latency = 0.1
  }

-- general shape for stream
beatsShape = toShape beats
