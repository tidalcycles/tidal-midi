module Sound.Tidal.MIDI.Minilogue where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

minilogueController :: ControllerShape
minilogueController = ControllerShape { controls = [
		mCC vco1_pitch_p 2,
		mCC vco2_pitch_p 3,
		mCC vco1_shape_p 4,
		mCC vco2_shape_p 5,
		mCC vco1_level_p 7,
		mCC vco2_level_p 8,
		mCC noise_p 1,
		mCC cross_mod_depth_p 9,
		mCC pitch_eg_int_p 10,
		mCC cutoff_p 11,
		mCC resonance_p 12,
		mCC filter_eg_int_p 13,
		mCC amp_attack_p 16,
		mCC amp_decay_p 17,
		mCC amp_sustant_p 18,
		mCC amp_release_p 19,
		mCC eg_attack_p 20,
		mCC eg_decay_p 21,
		mCC eg_sustant_p 22,
		mCC eg_release_p 23,
		mCC lfo_rate_p 24,
		mCC lfo_depth_p 25,
		mCC voice_depth_p 27,
		mCC delay_cutoff_p 29,
		mCC delay_time_p 30,
		mCC delay_feedback_p 31,
		mCC vco1_octave_p 64,
		mCC vco2_octave_p 65,
		mCC vco1_wave_p 66,
		mCC vco2_wave_p 67,
		mCC sync_p 80,
		mCC ring_p 81,
		mCC velocity_p 82,
		mCC key_track_p 83,
		mCC filter_type_p 84,
		mCC delay_output_routing_p 88,
		mCC lfo_target_p 90,
		mCC lfo_amount_p 91,
		mCC lfo_wave_p 92
	],
 --duration = ("dur", 0.05),
 --velocity = ("vel", 0.5),
 latency = 0.1
}

minilogue = toShape minilogueController

(vco1_pitch, vco1_pitch_p) = pF "vco1_pitch" (Just 0)
(vco2_pitch, vco2_pitch_p) = pF "vco2_pitch" (Just 0)
(vco1_shape, vco1_shape_p) = pF "vco1_shape" (Just 0)
(vco2_shape, vco2_shape_p) = pF "vco2_shape" (Just 0)
(vco1_level, vco1_level_p) = pF "vco1_level" (Just 0)
(vco2_level, vco2_level_p) = pF "vco2_level" (Just 0)
(noise, noise_p) = pF "noise" (Just 0)
(cross_mod_depth, cross_mod_depth_p) = pF "cross_mod_depth" (Just 0)
(pitch_eg_int, pitch_eg_int_p) = pF "pitch_eg_int" (Just 0)
--(cutoff, cutoff_p) = pF "cutoff" (Just 0)
--(resonance, resonance_p) = pF "resonance" (Just 0)
(filter_eg_int, filter_eg_int_p) = pF "filter_eg_int" (Just 0)
(amp_attack, amp_attack_p) = pF "amp_attack" (Just 0)
(amp_decay, amp_decay_p) = pF "amp_decay" (Just 0)
(amp_sustant, amp_sustant_p) = pF "amp_sustant" (Just 0)
(amp_release, amp_release_p) = pF "amp_release" (Just 0)
(eg_attack, eg_attack_p) = pF "eg_attack" (Just 0)
(eg_decay, eg_decay_p) = pF "eg_decay" (Just 0)
(eg_sustant, eg_sustant_p) = pF "eg_sustant" (Just 0)
(eg_release, eg_release_p) = pF "eg_release" (Just 0)
(lfo_rate, lfo_rate_p) = pF "lfo_rate" (Just 0)
(lfo_depth, lfo_depth_p) = pF "lfo_depth" (Just 0)
(voice_depth, voice_depth_p) = pF "voice_depth" (Just 0)
(delay_cutoff, delay_cutoff_p) = pF "delay_cutoff" (Just 0)
(delay_time, delay_time_p) = pF "delay_time" (Just 0)
(delay_feedback, delay_feedback_p) = pF "delay_feedback" (Just 0)
(vco1_octave, vco1_octave_p) = pF "vco1_octave" (Just 0)
(vco2_octave, vco2_octave_p) = pF "vco2_octave" (Just 0)
(vco1_wave, vco1_wave_p) = pF "vco1_wave" (Just 0)
(vco2_wave, vco2_wave_p) = pF "vco2_wave" (Just 0)
(sync, sync_p) = pF "sync" (Just 0)
(ring, ring_p) = pF "ring" (Just 0)
--(velocity, velocity_p) = pF "velocity" (Just 0)
(key_track, key_track_p) = pF "key_track" (Just 0)
(filter_type, filter_type_p) = pF "filter_type" (Just 0)
(delay_output_routing, delay_output_routing_p) = pF "delay_output_routing" (Just 0)
(lfo_target, lfo_target_p) = pF "lfo_target" (Just 0)
(lfo_amount, lfo_amount_p) = pF "lfo_amount" (Just 0)
(lfo_wave, lfo_wave_p) = pF "lfo_wave" (Just 0)
