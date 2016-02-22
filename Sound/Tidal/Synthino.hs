module Sound.Tidal.Synthino where

import Sound.Tidal.Context hiding (latency)
import Sound.Tidal.MIDI.Control

synthino :: ControllerShape
synthino = ControllerShape {
  controls = [
     mCC attack_p 73,
     mCC decay_p 75,
     mCC sustain_p 79,
     mCC release_p 72,
     mCC waveform_p 70,
     mCC pitchlforate_p 76,
     mCC pitchlfodepth_p 1,
     mCC lfowaveform_p 12,
     mCC filterlforate_p 13,
     mCC filterlfodepth_p 91,
     mCC resonance_p 71,
     mCC cutoff_p 74,
     mCC bpm_p 16,
     mCC arplength_p 17,
     mCC arptranspose_p 18,
     mCC gain_p 7,
     mCC mute_p 123
     ],
  latency = 0.1
  }

synthinoShape = toShape synthino

(waveform, waveform_p)             = pF "waveform" (Just 0)
(pitchlforate, pitchlforate_p)     = pF "pitchlforate" (Just 0)
(pitchlfodepth, pitchlfodepth_p)   = pF "pitchlfodepth" (Just 0)
(lfowaveform, lfowaveform_p)       = pF "lfowaveform" (Just 0)
(filterlforate, filterlforate_p)   = pF "filterlforate" (Just 0)
(filterlfodepth, filterlfodepth_p) = pF "filterlfodepth" (Just 0)
(bpm, bpm_p)                       = pF "bpm" (Just 0)
(arplength, arplength_p)           = pF "arplength" (Just 0)
(arptranspose, arptranspose_p)     = pF "arptranspose" (Just 0)
