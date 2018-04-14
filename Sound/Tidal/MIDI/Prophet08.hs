module Sound.Tidal.MIDI.Prophet08 where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

prophetController :: ControllerShape
prophetController = ControllerShape { controls = [
	mCC osc1_frequency_p 20,
	mCC osc1_freq_fine_p 21,
	mCC osc1_shape_p 22,
	mCC osc1_glide_p 23,
	mCC osc2_frequency_p 24,
	mCC osc2_freq_fine_p 25,
	mCC osc2_shape_p 26,
	mCC osc2_glide_p 27,
	mCC osc_mix_p 28,
	mCC noise_p 29,
	mCC filter_frequency_p 102,
	mCC resonance_p 103,
	mCC filter_key_amt_p 104,
	mCC filter_audio_mod_p 105,
	mCC filter_env_amt_p 106,
	mCC filter_env_vel_amt_p 107,
	mCC filter_delay_p 108,
	mCC filter_attack_p 109,
	mCC filter_decay_p 110,
	mCC filter_sustain_p 111,
	mCC filter_release_p 112 ,
	mCC vca_level_p 113,
	mCC pan_spread_p 114,
	mCC amp_env_amt_p 115,
	mCC amp_velocity_amt_p 116,
	mCC amp_delay_p 117,
	mCC amp_attack_p 118,
	mCC amp_decay_p 119,
	mCC amp_sustain_p 75,
	mCC amp_release_p 76,
	mCC env3_destination_p 85,
	mCC env3_amt_p 86,
	mCC env3_velocity_amt_p 87,
	mCC env3_delay_p 88,
	mCC env3_attack_p 89,
	mCC env3_decay_p 90,
	mCC env3_sustain_p 77,
	mCC env3_release_p 78,
	mCC bpm_p 14,
	mCC clock_divide_p 15
	],
 --duration = ("dur", 0.05),
 --velocity = ("vel", 0.5),
 latency = 0.1
}

prophet = toShape prophetController

(osc1_frequency, osc1_frequency_p) = pF "osc1_frequency" (Just 0)
(osc1_freq_fine, osc1_freq_fine_p) = pF "osc1_freq_fine" (Just 0)
(osc1_shape, osc1_shape_p) = pF "osc1_shape" (Just 0)
(osc1_glide, osc1_glide_p) = pF "osc1_glide" (Just 0)
(osc2_frequency, osc2_frequency_p) = pF "osc2_frequency" (Just 0)
(osc2_freq_fine, osc2_freq_fine_p) = pF "osc2_freq_fine" (Just 0)
(osc2_shape, osc2_shape_p) = pF "osc2_shape" (Just 0)
(osc2_glide, osc2_glide_p) = pF "osc2_glide" (Just 0)
(osc_mix, osc_mix_p) = pF "osc_mix" (Just 0)
(noise, noise_p) = pF "noise" (Just 0)
(filter_frequency, filter_frequency_p) = pF "filter_frequency" (Just 0)
--(resonance, resonance_p) = pF "resonance" (Just 0)
(filter_key_amt, filter_key_amt_p) = pF "filter_key_amt" (Just 0)
(filter_audio_mod, filter_audio_mod_p) = pF "filter_audio_mod" (Just 0)
(filter_env_amt, filter_env_amt_p) = pF "filter_env_amt" (Just 0)
(filter_env_vel_amt, filter_env_vel_amt_p) = pF "filter_env_vel_amt" (Just 0)
(filter_delay, filter_delay_p) = pF "filter_delay" (Just 0)
(filter_attack, filter_attack_p) = pF "filter_attack" (Just 0)
(filter_decay, filter_decay_p) = pF "filter_decay" (Just 0)
(filter_sustain, filter_sustain_p) = pF "filter_sustain" (Just 0)
(filter_release, filter_release_p) = pF "filter_release" (Just 0)
(vca_level, vca_level_p) = pF "vca_level" (Just 0)
(pan_spread, pan_spread_p) = pF "pan_spread" (Just 0)
(amp_env_amt, amp_env_amt_p) = pF "amp_env_amt" (Just 0)
(amp_velocity_amt, amp_velocity_amt_p) = pF "amp_velocity_amt" (Just 0)
(amp_delay, amp_delay_p) = pF "amp_delay" (Just 0)
(amp_attack, amp_attack_p) = pF "amp_attack" (Just 0)
(amp_decay, amp_decay_p) = pF "amp_decay" (Just 0)
(amp_sustain, amp_sustain_p) = pF "amp_sustain" (Just 0)
(amp_release, amp_release_p) = pF "amp_release" (Just 0)
(env3_destination, env3_destination_p) = pF "env3_destination" (Just 0)
(env3_amt, env3_amt_p) = pF "env3_amt" (Just 0)
(env3_velocity_amt, env3_velocity_amt_p) = pF "env3_velocity_amt" (Just 0)
(env3_delay, env3_delay_p) = pF "env3_delay" (Just 0)
(env3_attack, env3_attack_p) = pF "env3_attack" (Just 0)
(env3_decay, env3_decay_p) = pF "env3_decay" (Just 0)
(env3_sustain, env3_sustain_p) = pF "env3_sustain" (Just 0)
(env3_release, env3_release_p) = pF "env3_release" (Just 0)
(bpm, bpm_p) = pF "bpm" (Just 0)
(clock_divide, clock_divide_p) = pF "clock_divide" (Just 0)
