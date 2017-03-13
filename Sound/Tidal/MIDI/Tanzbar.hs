module Sound.Tidal.MIDI.Tanzbar where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control


(bd1_attack, bd1_attack_p) = pF "bd1_attack" (Just 0)
(bd1_decay, bd1_decay_p) = pF "bd1_decay" (Just 0)
(bd1_tune, bd1_tune_p) = pF "bd1_tune" (Just 0)
(bd1_noise, bd1_noise_p) = pF "bd1_noise" (Just 0)
(bd1_filter, bd1_filter_p) = pF "bd1_filter" (Just 0)
(bd1_dist, bd1_dist_p) = pF "bd1_dist" (Just 0)
(bd1_trigger, bd1_trigger_p) = pF "bd1_trigger" (Just 0)
(bd2_decay, bd2_decay_p) = pF "bd2_decay" (Just 0)
(bd2_tune, bd2_tune_p) = pF "bd2_tune" (Just 0)
(bd2_tone, bd2_tone_p) = pF "bd2_tone" (Just 0)
(sd_tune, sd_tune_p) = pF "sd_tune" (Just 0)
(sd_d_tune, sd_d_tune_p) = pF "sd_d_tune" (Just 0)
(sd_snappy, sd_snappy_p) = pF "sd_snappy" (Just 0)
(sd_sn_decay, sd_sn_decay_p) = pF "sd_sn_decay" (Just 0)
(sd_tone, sd_tone_p) = pF "sd_tone" (Just 0)
(sd_tone_decay, sd_tone_decay_p) = pF "sd_tone_decay" (Just 0)
(sd_pitch, sd_pitch_p) = pF "sd_pitch" (Just 0)
(rs_tune, rs_tune_p) = pF "rs_tune" (Just 0)
(cy_decay, cy_decay_p) = pF "cy_decay" (Just 0)
(cy_tone, cy_tone_p) = pF "cy_tone" (Just 0)
(cy_tune, cy_tune_p) = pF "cy_tune" (Just 0)
(oh_decay, oh_decay_p) = pF "oh_decay" (Just 0)
(hh_tune, hh_tune_p) = pF "hh_tune" (Just 0)
(hh_decay, hh_decay_p) = pF "hh_decay" (Just 0)
(cl_tune, cl_tune_p) = pF "cl_tune" (Just 0)
(cl_decay, cl_decay_p) = pF "cl_decay" (Just 0)
(cp_decay, cp_decay_p) = pF "cp_decay" (Just 0)
(cp_filter, cp_filter_p) = pF "cp_filter" (Just 0)
(cp_attack, cp_attack_p) = pF "cp_attack" (Just 0)
(cp_trigger, cp_trigger_p) = pF "cp_trigger" (Just 0)
(htc_tune, htc_tune_p) = pF "htc_tune" (Just 0)
(htc_decay, htc_decay_p) = pF "htc_decay" (Just 0)
(htc_noise_on_off, htc_noise_on_off_p) = pF "htc_noise_on_off" (Just 0)
(htc_tom_conga, htc_tom_conga_p) = pF "htc_tom_conga" (Just 0)
(mtc_tune, mtc_tune_p) = pF "mtc_tune" (Just 0)
(mtc_decay, mtc_decay_p) = pF "mtc_decay" (Just 0)
(mtc_noise_on_off, mtc_noise_on_off_p) = pF "mtc_noise_on_off" (Just 0)
(mtc_tom_conga, mtc_tom_conga_p) = pF "mtc_tom_conga" (Just 0)
(ltc_tune, ltc_tune_p) = pF "ltc_tune" (Just 0)
(ltc_decay, ltc_decay_p) = pF "ltc_decay" (Just 0)
(ltc_noise_on_off, ltc_noise_on_off_p) = pF "ltc_noise_on_off" (Just 0)
(ltc_tom_conga, ltc_tom_conga_p) = pF "ltc_tom_conga" (Just 0)
(tom_noise, tom_noise_p) = pF "tom_noise" (Just 0)
(cb_tune, cb_tune_p) = pF "cb_tune" (Just 0)
(cb_decay, cb_decay_p) = pF "cb_decay" (Just 0)
(ma_decay, ma_decay_p) = pF "ma_decay" (Just 0)
(set_select, set_select_p) = pF "set_select" (Just 0)
(track_delay_cv1, track_delay_cv1_p) = pF "track_delay_cv1" (Just 0)
(track_delay_cv23, track_delay_cv23_p) = pF "track_delay_cv23" (Just 0)
(track_delay_bd1, track_delay_bd1_p) = pF "track_delay_bd1" (Just 0)
(track_delay_bd2, track_delay_bd2_p) = pF "track_delay_bd2" (Just 0)
(track_delay_sd, track_delay_sd_p) = pF "track_delay_sd" (Just 0)
(track_delay_rs, track_delay_rs_p) = pF "track_delay_rs" (Just 0)
(track_delay_cy, track_delay_cy_p) = pF "track_delay_cy" (Just 0)
(track_delay_oh, track_delay_oh_p) = pF "track_delay_oh" (Just 0)
(track_delay_hh, track_delay_hh_p) = pF "track_delay_hh" (Just 0)
(track_delay_cl, track_delay_cl_p) = pF "track_delay_cl" (Just 0)
(track_delay_cp, track_delay_cp_p) = pF "track_delay_cp" (Just 0)
(track_delay_ltc, track_delay_ltc_p) = pF "track_delay_ltc" (Just 0)
(track_delay_mtc, track_delay_mtc_p) = pF "track_delay_mtc" (Just 0)
(track_delay_htc, track_delay_htc_p) = pF "track_delay_htc" (Just 0)
(track_delay_cb, track_delay_cb_p) = pF "track_delay_cb" (Just 0)
(track_delay_ma, track_delay_ma_p) = pF "track_delay_ma" (Just 0)
(bd1, bd1_p) = pF "bd1" (Just 0)
(bd2, bd2_p) = pF "bd2" (Just 0)
(sd, sd_p) = pF "sd" (Just 0)
(rs, rs_p) = pF "rs" (Just 0)
(cy, cy_p) = pF "cy" (Just 0)
(oh, oh_p) = pF "oh" (Just 0)
(hh, hh_p) = pF "hh" (Just 0)
(cl, cl_p) = pF "cl" (Just 0)
(cp, cp_p) = pF "cp" (Just 0)
(ltc, ltc_p) = pF "ltc" (Just 0)
(mtc, mtc_p) = pF "mtc" (Just 0)
(htc, htc_p) = pF "htc" (Just 0)
(cb, cb_p) = pF "cb" (Just 0)
(ma, ma_p) = pF "ma" (Just 0)

tanzController :: ControllerShape
tanzController = ControllerShape {
    controls = [
    mCC bd1_attack_p 2,
    mCC bd1_decay_p 64,
    mCC bd1_tune_p 3,
    mCC bd1_noise_p 4,
    mCC bd1_filter_p 5,
    mCC bd1_dist_p 6,
    mCC bd1_trigger_p 66,
    mCC bd2_decay_p 8,
    mCC bd2_tune_p 9,
    mCC bd2_tone_p 10,
    mCC sd_tune_p 11,
    mCC sd_d_tune_p 12,
    mCC sd_snappy_p 13,
    mCC sd_sn_decay_p 67,
    mCC sd_tone_p 14,
    mCC sd_tone_decay_p 68,
    mCC sd_pitch_p 69,
    mCC rs_tune_p 88,
    mCC cy_decay_p 70,
    mCC cy_tone_p 15,
    mCC cy_tune_p 71,
    mCC oh_decay_p 75,
    mCC hh_tune_p 73,
    mCC hh_decay_p 74,
    mCC cl_tune_p 16,
    mCC cl_decay_p 17,
    mCC cp_decay_p 75,
    mCC cp_filter_p 18,
    mCC cp_attack_p 76,
    mCC cp_trigger_p 77,
    mCC htc_tune_p 19,
    mCC htc_decay_p 20,
    mCC htc_noise_on_off_p 78,
    mCC htc_tom_conga_p 79,
    mCC mtc_tune_p 21,
    mCC mtc_decay_p 22,
    mCC mtc_noise_on_off_p 80,
    mCC mtc_tom_conga_p 81,
    mCC ltc_tune_p 23,
    mCC ltc_decay_p 24,
    mCC ltc_noise_on_off_p 82,
    mCC ltc_tom_conga_p 83,
    mCC tom_noise_p 84,
    mCC cb_tune_p 85,
    mCC cb_decay_p 86,
    mCC ma_decay_p 87,
    mCC set_select_p 0,
    mCC track_delay_cv1_p 89,
    mCC track_delay_cv23_p 90,
    mCC track_delay_bd1_p 91,
    mCC track_delay_bd2_p 92,
    mCC track_delay_sd_p 93,
    mCC track_delay_rs_p 94,
    mCC track_delay_cy_p 95,
    mCC track_delay_oh_p 96,
    mCC track_delay_hh_p 97,
    mCC track_delay_cl_p 98,
    mCC track_delay_cp_p 99,
    mCC track_delay_ltc_p 100,
    mCC track_delay_mtc_p 101,
    mCC track_delay_htc_p 102,
    mCC track_delay_cb_p 103,
    mCC track_delay_ma_p 104,
    mCC bd1_p 36,
    mCC bd2_p 37,
    mCC sd_p 38,
    mCC rs_p 39,
    mCC cy_p 40,
    mCC oh_p 41,
    mCC hh_p 42,
    mCC cl_p 43,
    mCC cp_p 44,
    mCC ltc_p 45,
    mCC mtc_p 46,
    mCC htc_p 47,
    mCC cb_p 48,
    mCC ma_p 49
       ],
    latency = 0.1
    }





tanz = midinote . (tanzN <$>)

tanzN :: String -> Int
tanzN "bd1" = 36
tanzN "bd2" = 37
tanzN "sd" = 38
tanzN "rs" = 39
tanzN "cy" = 40
tanzN "oh" = 41
tanzN "hh" = 42
tanzN "cl" = 43
tanzN "cp" = 44
tanzN "ltc" = 45
tanzN "mtc" = 46
tanzN "htc" = 47
tanzN "cb" = 48
tanzN "ma" = 49


-- general shape for stream
tanzShape = toShape tanzController
