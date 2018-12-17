module Sound.Tidal.MIDI.Circuit where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

circuitController :: ControllerShape
circuitController = ControllerShape { controls = [
		-- voice
		mCC polyphony_mode_p 3,
		mCC portamento_rate_p 5,
		mCC pre_glide_p 9,
		mCC keyboard_octave_p 13,

		-- oscillator
		mCC osc1_wave_p 19,
		mCC osc1_wave_interpolate_p 20,
		mCC osc1_pulse_width_index_p 21,
		mCC osc1_virtual_sync_depth_p 22,
		mCC osc1_density_p 24,
		mCC osc1_density_detune_p 25,
		mCC osc1_semitones_p 26,
		mCC osc1_cents_p 27,
		mCC osc2_pitchbend_p 28,
		mCC osc2_wave_p 29,
		mCC osc2_wave_interpolate_p 30,
		mCC osc2_pulse_width_index_p 31,
		mCC osc2_virtual_sync_depth_p 33,
		mCC osc2_density_p 35,
		mCC osc2_density_detune_p 36,
		mCC osc2_semitones_p 37,
		mCC osc2_cents_p 39,
		mCC osc2_pitchbend_p 40,

		-- mixer
		mCC osc1_level_p 51,
		mCC osc2_level_p 52,
		mCC ring_mod_level_p 54,
		mCC noise_level_p 56,
		mCC pre_fx_level_p 58,
		mCC post_fx_level_p 59,

		-- filter
		mCC routing_p 60,
		mCC drive_p 63,
		mCC drive_type_p 65,
		mCC filter_type_p 68,
		mCC frequency_p 74,
		mCC tracking_p 69,
		mCC filter_resonance_p 71,
		mCC q_normalize_p 78,
		mCC env2_to_frequency_p 79,

		-- envelope
		mCC env1_velocity_p 108,
		mCC env1_attack_p 73,
		mCC env1_decay_p 75,
		mCC env1_sustain_p 70,
		mCC env1_release_p 72,

		-- effects & eq
		mCC distortion_level_p 91,
		mCC chorus_level_p 93,

		-- macro knobs
		mCC macro_knob1_p 80,
		mCC macro_knob2_p 81,
		mCC macro_knob3_p 82,
		mCC macro_knob4_p 83,
		mCC macro_knob5_p 84,
		mCC macro_knob6_p 85,
		mCC macro_knob7_p 86,
		mCC macro_knob8_p 87,

		-- drums
		mCC drum1_patch_p 8,
		mCC drum1_level_p 12,
		mCC drum1_pitch_p 14,
		mCC drum1_decay_p 15,
		mCC drum1_distortion_p 16,
		mCC drum1_eq_p 17,
		mCC drum2_patch_p 18,
		mCC drum2_level_p 23,
		mCC drum2_pitch_p 34,
		mCC drum2_decay_p 40,
		mCC drum2_distortion_p 42,
		mCC drum2_eq_p 43,
		mCC drum3_patch_p 44,
		mCC drum3_level_p 45,
		mCC drum3_pitch_p 46,
		mCC drum3_decay_p 47,
		mCC drum3_distortion_p 48,
		mCC drum3_eq_p 49,
		mCC drum4_patch_p 50,
		mCC drum4_level_p 53,
		mCC drum4_pitch_p 55,
		mCC drum4_decay_p 57,
		mCC drum4_distortion_p 61,
		mCC drum4_eq_p 76,

		-- session control
		mCC synth1_reverb_p 88,
		mCC synth2_reverb_p 89,
		mCC drum1_reverb_p 90,
		mCC drum2_reverb_p 106,
		mCC drum3_reverb_p 109,
		mCC drum4_reverb_p 110,

		mCC synth1_delay_p 111,
		mCC synth2_delay_p 112,
		mCC drum1_delay_p 113,
		mCC drum2_delay_p 114,
		mCC drum3_delay_p 115,
		mCC drum4_delay_p 116,

		-- global lp/hp filter
		mCC globfilter_frequency_p 74,
		mCC globfilter_filter_resonance_p 71,

		-- mixer
		mCC synth1_mix_p 12,
		mCC synth2_mix_p 14
	],

 latency = 1.5 
}

circuit = toShape circuitController

(polyphony_mode, polyphony_mode_p) = pF "polyphony_mode" (Just 2) -- 0-2
(portamento_rate, portamento_rate_p) = pF "portamento_rate" (Just 0) -- 0-127
(pre_glide, pre_glide_p) = pF "pre_glide" (Just 64) -- 52-76
(keyboard_octave, keyboard_octave_p) = pF "keyboard_octave" (Just 64) -- 58-69

(osc1_wave, osc1_wave_p) = pF "osc1_wave" (Just 2) -- 0-29
(osc1_wave_interpolate, osc1_wave_interpolate_p) = pF "osc1_wave_interpolate" (Just 0) -- 0-127
(osc1_pulse_width_index, osc1_pulse_width_index_p) = pF "osc1_pulse_width_index" (Just 127) -- 0-127
(osc1_virtual_sync_depth, osc1_virtual_sync_depth_p) = pF "osc1_virtual_sync_depth" (Just 0) -- 0-127
(osc1_density, osc1_density_p) = pF "osc1_density" (Just 0) -- 0-127
(osc1_density_detune, osc1_density_detune_p) = pF "osc1_density_detune" (Just 0) -- 0-127
(osc1_semitones, osc1_semitones_p) = pF "osc1_semitones" (Just 64) -- 0-127
(osc1_cents, osc1_cents_p) = pF "osc1_cents" (Just 64) -- 0-127
(osc1_pitchbend, osc1_pitchbend_p) = pF "osc1_pitchbend" (Just 76) -- 52-76
(osc2_wave, osc2_wave_p) = pF "osc2_wave" (Just 2) -- 0-29
(osc2_wave_interpolate, osc2_wave_interpolate_p) = pF "osc2_wave_interpolate" (Just 0) -- 0-127
(osc2_pulse_width_index, osc2_pulse_width_index_p) = pF "osc2_pulse_width_index" (Just 63) -- 0-127
(osc2_virtual_sync_depth, osc2_virtual_sync_depth_p) = pF "osc2_virtual_sync_depth" (Just 0) -- 0-127
(osc2_density, osc2_density_p) = pF "osc2_density" (Just 0) -- 0-127
(osc2_density_detune, osc2_density_detune_p) = pF "osc2_density_detune" (Just 0) -- 0-127
(osc2_semitones, osc2_semitones_p) = pF "osc2_semitones" (Just 64) -- 0-127
(osc2_cents, osc2_cents_p) = pF "osc2_cents" (Just 64) -- 0-127
(osc2_pitchbend, osc2_pitchbend_p) = pF "osc2_pitchbend" (Just 76) -- 52-76

(osc1_level, osc1_level_p) = pF "osc1_level" (Just 127) -- 0-127
(osc2_level, osc2_level_p) = pF "osc2_level" (Just 0) -- 0-127
(ring_mod_level, ring_mod_level_p) = pF "ring_mod_level" (Just 0) -- 0-127
(noise_level, noise_level_p) = pF "noise_level" (Just 0) -- 0-127
(pre_fx_level, pre_fx_level_p) = pF "pre_fx_level" (Just 64) -- 52-82
(post_fx_level, post_fx_level_p) = pF "post_fx_level" (Just 64) -- 52-82

(routing, routing_p) = pF "routing" (Just 0) -- 0-2
(drive, drive_p) = pF "drive" (Just 0) -- 0-127
(drive_type, drive_type_p) = pF "drive_type" (Just 0) -- 0-6
(filter_type, filter_type_p) = pF "filter_type" (Just 1) -- 0-5
(frequency, frequency_p) = pF "frequency" (Just 127) -- 0-127
(tracking, tracking_p) = pF "tracking" (Just 127) -- 0-127
(resonance, filter_resonance_p) = pF "resonance" (Just 0) -- 0-127
(q_normalize, q_normalize_p) = pF "q_normalize" (Just 64) -- 0-127
(env2_to_frequency, env2_to_frequency_p) = pF "env2_to_frequency" (Just 64) -- 0-127

(env1_velocity, env1_velocity_p) = pF "env1_velocity" (Just 64) -- 0-127
(env1_attack, env1_attack_p) = pF "env1_attack" (Just 2) -- 0-127
(env1_decay, env1_decay_p) = pF "env1_decay" (Just 90) -- 0-127
(env1_sustain, env1_sustain_p) = pF "env1_sustain" (Just 127) -- 0-127
(env1_release, env1_release_p) = pF "env1_release" (Just 40) -- 0-127

(distortion_level, distortion_level_p) = pF "distortion_level" (Just 0) -- 0-127
(chorus_level, chorus_level_p) = pF "chorus_level" (Just 0) -- 0-127

(macro_knob1, macro_knob1_p) = pF "macro_knob1" (Just 0) -- 0-127
(macro_knob2, macro_knob2_p) = pF "macro_knob2" (Just 0) -- 0-127
(macro_knob3, macro_knob3_p) = pF "macro_knob3" (Just 0) -- 0-127
(macro_knob4, macro_knob4_p) = pF "macro_knob4" (Just 0) -- 0-127
(macro_knob5, macro_knob5_p) = pF "macro_knob5" (Just 0) -- 0-127
(macro_knob6, macro_knob6_p) = pF "macro_knob6" (Just 0) -- 0-127
(macro_knob7, macro_knob7_p) = pF "macro_knob7" (Just 0) -- 0-127
(macro_knob8, macro_knob8_p) = pF "macro_knob8" (Just 0) -- 0-127

(drum1_patch, drum1_patch_p) = pF "drum1_patch" (Just 0) -- 0-63
(drum1_level, drum1_level_p) = pF "drum1_level" (Just 0) -- 0-127
(drum1_pitch, drum1_pitch_p) = pF "drum1_pitch" (Just 64) -- 0-127
(drum1_decay, drum1_decay_p) = pF "drum1_decay" (Just 0) -- 0-127
(drum1_distortion, drum1_distortion_p) = pF "drum1_distortion" (Just 0) -- 0-127
(drum1_eq, drum1_eq_p) = pF "drum1_eq" (Just 64) -- 0-127
(drum2_patch, drum2_patch_p) = pF "drum2_patch" (Just 0) -- 0-63
(drum2_level, drum2_level_p) = pF "drum2_level" (Just 0) -- 0-127
(drum2_pitch, drum2_pitch_p) = pF "drum2_pitch" (Just 64) -- 0-127
(drum2_decay, drum2_decay_p) = pF "drum2_decay" (Just 0) -- 0-127
(drum2_distortion, drum2_distortion_p) = pF "drum2_distortion" (Just 0) -- 0-127
(drum2_eq, drum2_eq_p) = pF "drum2_eq" (Just 64) -- 0-127
(drum3_patch, drum3_patch_p) = pF "drum3_patch" (Just 0) -- 0-63
(drum3_level, drum3_level_p) = pF "drum3_level" (Just 0) -- 0-127
(drum3_pitch, drum3_pitch_p) = pF "drum3_pitch" (Just 64) -- 0-127
(drum3_decay, drum3_decay_p) = pF "drum3_decay" (Just 0) -- 0-127
(drum3_distortion, drum3_distortion_p) = pF "drum3_distortion" (Just 0) -- 0-127
(drum3_eq, drum3_eq_p) = pF "drum3_eq" (Just 64) -- 0-127
(drum4_patch, drum4_patch_p) = pF "drum4_patch" (Just 0) -- 0-63
(drum4_level, drum4_level_p) = pF "drum4_level" (Just 0) -- 0-127
(drum4_pitch, drum4_pitch_p) = pF "drum4_pitch" (Just 64) -- 0-127
(drum4_decay, drum4_decay_p) = pF "drum4_decay" (Just 0) -- 0-127
(drum4_distortion, drum4_distortion_p) = pF "drum4_distortion" (Just 0) -- 0-127
(drum4_eq, drum4_eq_p) = pF "drum4_eq" (Just 64) -- 0-127

(synth1_reverb, synth1_reverb_p) = pF "synth1_reverb" (Just 0) -- 0-127
(synth2_reverb, synth2_reverb_p) = pF "synth2_reverb" (Just 0) -- 0-127
(drum1_reverb, drum1_reverb_p) = pF "drum1_reverb" (Just 0) -- 0-127
(drum2_reverb, drum2_reverb_p) = pF "drum2_reverb" (Just 0) -- 0-127
(drum3_reverb, drum3_reverb_p) = pF "drum3_reverb" (Just 0) -- 0-127
(drum4_reverb, drum4_reverb_p) = pF "drum4_reverb" (Just 0) -- 0-127

(synth1_delay, synth1_delay_p) = pF "synth1_delay" (Just 0) -- 0-127
(synth2_delay, synth2_delay_p) = pF "synth2_delay" (Just 0) -- 0-127
(drum1_delay, drum1_delay_p) = pF "drum1_delay" (Just 0) -- 0-127
(drum2_delay, drum2_delay_p) = pF "drum2_delay" (Just 0) -- 0-127
(drum3_delay, drum3_delay_p) = pF "drum3_delay" (Just 0) -- 0-127
(drum4_delay, drum4_delay_p) = pF "drum4_delay" (Just 0) -- 0-127

(globfilter_frequency, globfilter_frequency_p) = pF "globfilter_frequency" (Just 64) -- 0-127
(globfilter_resonance, globfilter_filter_resonance_p) = pF "globfilter_resonance" (Just 30) -- 0-127

(synth1_mix, synth1_mix_p) = pF "synth1_mix" (Just 100) -- 0-127
(synth2_mix, synth2_mix_p) = pF "synth2_mix" (Just 100) -- 0-127
