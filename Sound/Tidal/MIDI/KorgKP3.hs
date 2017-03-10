module Sound.Tidal.MIDI.KorgKP3 where

import Sound.Tidal.Params

import Sound.Tidal.MIDI.Control

kp3 :: ControllerShape
kp3 =
  ControllerShape
  { controls =
      [ mCC xaxis_p 12
      , mCC yaxis_p 13
      , mCC padon_p 92
      , mCC level_p 93
      , mCC fxdepth_p 94
      , mCC holdbut_p 95
      ]
  , latency = 0.1
  }

oscKp3 = toShape kp3

(xaxis, xaxis_p) = pF "xaxis" (Just 0)

(yaxis, yaxis_p) = pF "yaxis" (Just 0)

(padon, padon_p) = pF "padon" (Just 0)

(level, level_p) = pF "level" (Just 0)

(fxdepth, fxdepth_p) = pF "fxdepth" (Just 0)

(holdbut, holdbut_p) = pF "holdbut" (Just 0)
