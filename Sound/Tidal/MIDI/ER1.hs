module Sound.Tidal.MIDI.ER1 where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

er1Controller :: ControllerShape
er1Controller = ControllerShape { controls = [
		-- part 1
		mCC part1_pitch_p 2,
		mCC part1_mod_depth_p 6,
		mCC part1_mod_speed_p 5,
		mCC part1_mod_type_p 4,
		mCC part1_wave_type_p 3,
		mCC part1_decay_p 8,
		mCC part1_level_p 7,
		mCC part1_pan_p 1,
		mCC part1_low_boost_p 0,

		-- part 2
		mCC part2_pitch_p 12,
		mCC part2_mod_depth_p 16,
		mCC part2_mod_speed_p 15,
		mCC part2_mod_type_p 14,
		mCC part2_wave_type_p 13,
		mCC part2_decay_p 18,
		mCC part2_level_p 17,
		mCC part2_pan_p 11,
		mCC part2_low_boost_p 10,

		-- part 3
		mCC part3_pitch_p 22,
		mCC part3_mod_depth_p 26,
		mCC part3_mod_speed_p 25,
		mCC part3_mod_type_p 24,
		mCC part3_wave_type_p 23,
		mCC part3_decay_p 28,
		mCC part3_level_p 27,
		mCC part3_pan_p 21,
		mCC part3_low_boost_p 20,

		-- part 4
		mCC part4_pitch_p 32,
		mCC part4_mod_depth_p 36,
		mCC part4_mod_speed_p 35,
		mCC part4_mod_type_p 34,
		mCC part4_wave_type_p 33,
		mCC part4_decay_p 38,
		mCC part4_level_p 37,
		mCC part4_pan_p 31,
		mCC part4_low_boost_p 30

	],

 latency = 1.5
}

er1 = toShape er1Controller

(part1_pitch, part1_pitch_p) = pF "part1_pitch" (Just 30) -- 0-127
(part1_mod_depth, part1_mod_depth_p) = pF "part1_mod_depth" (Just 30) -- 0-127
(part1_mod_speed, part1_mod_speed_p) = pF "part1_mod_speed" (Just 30) -- 0-127
(part1_mod_type, part1_mod_type_p) = pF "part1_mod_type" (Just 0) -- 0-127
(part1_wave_type, part1_wave_type_p) = pF "part1_wave_type" (Just 0) -- 0-127
(part1_decay, part1_decay_p) = pF "part1_decay" (Just 30) -- 0-127
(part1_level, part1_level_p) = pF "part1_level" (Just 30) -- 0-127
(part1_pan, part1_pan_p) = pF "part1_pan" (Just 64) -- 0-127
(part1_low_boost, part1_low_boost_p) = pF "part1_low_boost" (Just 30) -- 0-127

(part2_pitch, part2_pitch_p) = pF "part2_pitch" (Just 30) -- 0-127
(part2_mod_depth, part2_mod_depth_p) = pF "part2_mod_depth" (Just 30) -- 0-127
(part2_mod_speed, part2_mod_speed_p) = pF "part2_mod_speed" (Just 30) -- 0-127
(part2_mod_type, part2_mod_type_p) = pF "part2_mod_type" (Just 0) -- 0-127
(part2_wave_type, part2_wave_type_p) = pF "part2_wave_type" (Just 0) -- 0-127
(part2_decay, part2_decay_p) = pF "part2_decay" (Just 30) -- 0-127
(part2_level, part2_level_p) = pF "part2_level" (Just 30) -- 0-127
(part2_pan, part2_pan_p) = pF "part2_pan" (Just 64) -- 0-127
(part2_low_boost, part2_low_boost_p) = pF "part2_low_boost" (Just 30) -- 0-127

(part3_pitch, part3_pitch_p) = pF "part3_pitch" (Just 30) -- 0-127
(part3_mod_depth, part3_mod_depth_p) = pF "part3_mod_depth" (Just 30) -- 0-127
(part3_mod_speed, part3_mod_speed_p) = pF "part3_mod_speed" (Just 30) -- 0-127
(part3_mod_type, part3_mod_type_p) = pF "part3_mod_type" (Just 0) -- 0-127
(part3_wave_type, part3_wave_type_p) = pF "part3_wave_type" (Just 0) -- 0-127
(part3_decay, part3_decay_p) = pF "part3_decay" (Just 30) -- 0-127
(part3_level, part3_level_p) = pF "part3_level" (Just 30) -- 0-127
(part3_pan, part3_pan_p) = pF "part3_pan" (Just 64) -- 0-127
(part3_low_boost, part3_low_boost_p) = pF "part3_low_boost" (Just 30) -- 0-127

(part4_pitch, part4_pitch_p) = pF "part4_pitch" (Just 30) -- 0-127
(part4_mod_depth, part4_mod_depth_p) = pF "part4_mod_depth" (Just 30) -- 0-127
(part4_mod_speed, part4_mod_speed_p) = pF "part4_mod_speed" (Just 30) -- 0-127
(part4_mod_type, part4_mod_type_p) = pF "part4_mod_type" (Just 0) -- 0-127
(part4_wave_type, part4_wave_type_p) = pF "part4_wave_type" (Just 0) -- 0-127
(part4_decay, part4_decay_p) = pF "part4_decay" (Just 30) -- 0-127
(part4_level, part4_level_p) = pF "part4_level" (Just 30) -- 0-127
(part4_pan, part4_pan_p) = pF "part4_pan" (Just 64) -- 0-127
(part4_low_boost, part4_low_boost_p) = pF "part4_low_boost" (Just 30) -- 0-127

