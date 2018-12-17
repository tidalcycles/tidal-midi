module Sound.Tidal.MIDI.VolcaKick where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control


volcakickController :: ControllerShape
volcakickController =
  ControllerShape
  { controls =
      [ mCC pulsecolour_p 40
      , mCC pulselevel_p 41
      , mCC ampattack_p 42
      , mCC ampdecay_p 43
      , mCC drive_p 44
      , mCC tone_p 45
      , mCC pitch_p 46
      , mCC bend_p 47
      , mCC time_p 48
      , mCC accent_p 49
      ]
  , latency = 0.1
  }

(pulsecolour, pulsecolour_p) = pF "pulsecolour" (Just 0)

(pulselevel, pulselevel_p) = pF "pulselevel" (Just 0)

(ampattack, ampattack_p) = pF "ampattack" (Just 0)

(ampdecay, ampdecay_p) = pF "ampdecay" (Just 0)

(drive, drive_p) = pF "drive" (Just 0)

(tone, tone_p) = pF "tone" (Just 0)

(pitch, pitch_p) = pF "pitch" (Just 0)

(bend, bend_p) = pF "bend" (Just 0)

(time, time_p) = pF "time" (Just 0)

(accent, accent_p) = pF "accent" (Just 0)

VolcaKick = toShape volcakickController
