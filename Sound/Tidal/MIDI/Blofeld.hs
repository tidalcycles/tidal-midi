module Sound.Tidal.MIDI.Blofeld where

import Sound.Tidal.MIDI.Control
import Sound.Tidal.Params hiding (octave_p, semitone_p)

bloController :: ControllerShape
bloController =
  ControllerShape
  { controls =
      [ mCC portamento_p 5
      , mCC expression_p 11
      , CC lfoshape_p 15 (0, 5) passThru -- 0..5 - sine,triangle,square,saw,random,sample&hold
      , mCC lforate_p 16
      , CC lfosync_p 17 (0, 1) passThru -- 0 off, 1 on
      , mCC lfodelay_p 18
      , CC octave_p 27 (16, 112) passThru -- 16, 28, 40 .. 112 - 128' .. 1/2'
      , CC semitone_p 28 (52, 76) passThru -- 52 .. 76 - -12 - +12 semitones
      , mCC detune_p 29
      , mCC osc1fm_p 30
      , CC osc1shape_p 31 (0, 5) passThru -- 0..5 - pulse, saw, tri, sine, alt 1, alt 2
      , mCC osc1pw_p 33
      , mCC osc1pwm_p 34
      , mCC osc1vol_p 52
      , mCC osc1pan_p 53
      , mCC ringmod_p 54
      , mCC ringpan_p 55
      , mCC noise_p 60
      , mCC noisepan_p 61
      , mCC noisecol_p 62
      , mCC cutoff_p 69
      , mCC attack_p 101
      , mCC decay_p 102
      , mCC sustain_p 103
      , mCC release_p 106
      ]
  , latency = 0.1
  }

blofeld = toShape bloController

(_, octave_p) = pF "octave" (Just 0)

(_, semitone_p) = pF "semitone" (Just 0.5)

(lforate, lforate_p) = pF "lforate" (Just 0)

(osc1fm, osc1fm_p) = pF "osc1fm" (Just 0)

(osc1shape, osc1shape_p) = pF "osc1shape" (Just 0)

(osc1pw, osc1pw_p) = pF "osc1pw" (Just 0)

(osc1pwm, osc1pwm_p) = pF "osc1pwm" (Just 0)

(osc1vol, osc1vol_p) = pF "osc1vol" (Just 0)

(osc1pan, osc1pan_p) = pF "osc1pan" (Just 0)

(ringmod, ringmod_p) = pF "ringmod" (Just 0)

(ringpan, ringpan_p) = pF "ringpan" (Just 0)

(noise, noise_p) = pF "noise" (Just 0)

(noisepan, noisepan_p) = pF "noisepan" (Just 0)

(noisecol, noisecol_p) = pF "noisecol" (Just 0)
