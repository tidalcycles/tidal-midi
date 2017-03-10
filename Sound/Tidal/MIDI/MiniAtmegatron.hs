{-|
miniAtmegatron - Soulsby Synthesizers

http://soulsbysynths.com/wp-content/uploads/2016/08/Mini-Manual.pdf, page 15

-}
module Sound.Tidal.MIDI.MiniAtmegatron where

import Sound.Tidal.MIDI.Control
import Sound.Tidal.Params

matmController :: ControllerShape
matmController =
  ControllerShape
  { controls =
      [ mCC modwheel_p 1
      , mCC pan_p 10
      , mCC expression_p 11
      , mCC sustainpedal_p 64
      , mCC cutoff_p 74
      , mCC filtenv_p 16
      , mCC flfo_p 93
      , mCC alfo_p 92
      , mCC shape_p 17
      , mCC resonance_p 71
      , mCC penv_p 94
      , mCC plfo_p 1
      , mCC pwm_p 91
      , mCC flange_p 95
      , mCC fwave_p 30
      , mCC ffilt_p 31
      , mCC ffenv_p 32
      , mCC faenv_p 33
      , mCC mlfoshape_p 34
      , mCC lfospeed_p 79
      , mCC porta_p 5
      ]
  , latency = 0.1
  }

matm = toShape matmController

(fen, filtenv_p) = pF "fen" (Just 0)

(flo, flfo_p) = pF "flo" (Just 0)

(alo, alfo_p) = pF "alo" (Just 0)

(pen, penv_p) = pF "pen" (Just 0)

(plo, plfo_p) = pF "plo" (Just 0)

(pwm, pwm_p) = pF "pwm" (Just 0)

(fln, flange_p) = pF "fln" (Just 0)

(fwv, fwave_p) = pF "fwv" (Just 0)

(ffl, ffilt_p) = pF "ffl" (Just 0)

(ffe, ffenv_p) = pF "ffe" (Just 0)

(fae, faenv_p) = pF "fae" (Just 0)

(lfs, mlfoshape_p) = pF "lfs" (Just 0)

(lss, lfospeed_p) = pF "lss" (Just 0)

(por, porta_p) = pF "por" (Just 0)
