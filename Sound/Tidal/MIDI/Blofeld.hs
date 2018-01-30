module Sound.Tidal.MIDI.Blofeld where

import Sound.Tidal.MIDI.Control
import Sound.Tidal.Params 

-- Blofeld midi mapping --

-- mod wheel:		MIDI CC 1,	0...127
-- breath control:	MIDI CC 2, 	0...127
-- foot control:	MIDI CC 4,	0...127
-- glide rate:		MIDI CC 5,	0...127
-- channel volume:	MIDI CC 7,	0...127
-- pan: 		MIDI CC 10,	0...127
--
-- arp range: 		MIDI CC 12,	0...9	
-- arp length: 		MIDI CC 13,	0...15	
-- arp active: 		MIDI CC 14,	0...3	
--
-- lfo 1 shape:		MIDI CC 15,	0...5	
-- lfo 1 speed:		MIDI CC 16,	0...127	
-- lfo 1 sync:		MIDI CC 17,	0...1	
-- lfo 1 delay:		MIDI CC 18,	0...127	
--
-- lfo 2 shape:		MIDI CC 19,	0...5	
-- lfo 2 speed:		MIDI CC 20,	0...127	
-- lfo 2 sync:		MIDI CC 21,	0...1	
-- lfo 2 delay:		MIDI CC 22,	0...127	
--
-- lfo 3 shape:		MIDI CC 23,	0...5	
-- lfo 3 speed:		MIDI CC 24,	0...127	
-- lfo 3 sync:		MIDI CC 25,	0...1	
-- lfo 3 delay:		MIDI CC 26,	0...127	
--
-- osc 1 octave:	MIDI CC 27,	16,28,40...112
-- osc 1 semitone:	MIDI CC 28,	52...76
-- osc 1 detune:	MIDI CC 29,	0...127
-- osc 1 fm:		MIDI CC 30,	0...127
-- osc 1 shape:		MIDI CC 31,	0...5
--
-- bank select LSB:	MIDI CC 32,	0...n
--
-- osc 1 pw:		MIDI CC 33, 	0...127
-- osc 1 pwm:		MIDI CC 34, 	0...127
--
-- osc 2 octave:	MIDI CC 35,	16,28,40...112
-- osc 2 semitone:	MIDI CC 36,	52...76
-- osc 2 detune:	MIDI CC 37,	0...127
-- osc 2 fm:		MIDI CC 38,	0...127
-- osc 2 shape:		MIDI CC 39,	0...5
-- osc 2 pw:		MIDI CC 40, 	0...127
-- osc 2 pwm:		MIDI CC 41, 	0...127
--
-- osc 3 octave:	MIDI CC 42,	16,28,40...112
-- osc 3 semitone:	MIDI CC 43,	52...76
-- osc 3 detune:	MIDI CC 44,	0...127
-- osc 3 fm:		MIDI CC 45,	0...127
-- osc 3 shape:		MIDI CC 46,	0...5
-- osc 3 pw:		MIDI CC 47, 	0...127
-- osc 3 pwm:		MIDI CC 48, 	0...127
--
-- sync:		MIDI CC 49,	0...1
-- pitchmod:		MIDI CC 50,	0...127
-- glide mode:		MIDI CC 51,	0...9
--
-- osc 1 level:		MIDI CC 52, 	0...127
-- osc 1 balance:	MIDI CC 53, 	0...127
--
-- ringmod level: 	MIDI CC 54, 	0...127
-- ringmod bal: 	MIDI CC 55, 	0...127
--
-- osc 2 level:		MIDI CC 56, 	0...127
-- osc 2 balance:	MIDI CC 57, 	0...127
--
-- osc 3 level:		MIDI CC 58, 	0...127
-- osc 3 balance:	MIDI CC 59, 	0...127
--
-- noise level:		MIDI CC 60, 	0...127
-- noise balance:	MIDI CC 61, 	0...127
-- noise colour:	MIDI CC 62, 	0...127
--
-- sustain pedal:	MIDI CC 64,	0...127
-- glide active:	MIDI CC 65,	0...127
-- sostenuto:		MIDI CC 66,	0...127
--
-- routing:		MIDI CC 67,	0...127
-- filter 1 type:	MIDI CC 68,	0...10
-- filter 1 cutoff:	MIDI CC 69,	0...127
-- filter 1 resonance:	MIDI CC 70,	0...127
-- filter 1 drive:	MIDI CC 71,	0...127
-- filter 1 keytrack:	MIDI CC 72,	0...127
-- filter 1 env amnt:	MIDI CC 73,	0...127
-- filter 1 env vel:	MIDI CC 74,	0...127
-- filter 1 cutoff mod:	MIDI CC 75,	0...127
-- filter 1 fm:		MIDI CC 76,	0...127
-- filter 1 pan:	MIDI CC 77,	0...127
-- filter 1 panmod:	MIDI CC 78,	0...127
--
-- filter 2 type:	MIDI CC 79,	0...10
-- filter 2 cutoff:	MIDI CC 80,	0...127
-- filter 2 resonance:	MIDI CC 81,	0...127
-- filter 2 drive:	MIDI CC 82,	0...127
-- filter 2 keytrack:	MIDI CC 83,	0...127
-- filter 2 env amnt:	MIDI CC 84,	0...127
-- filter 2 env vel:	MIDI CC 85,	0...127
-- filter 2 cutoff mod:	MIDI CC 86,	0...127
-- filter 2 fm:		MIDI CC 87,	0...127
-- filter 2 pan:	MIDI CC 88,	0...127
-- filter 2 panmod:	MIDI CC 89,	0...127
--
-- amp volume:		MIDI CC 90,	0...127
-- amp velocity:	MIDI CC 91,	0...127
-- amp mod:		MIDI CC 92,	0...127
--
-- fx 1 mix:		MIDI CC 93,	0...127
-- fx 2 mix:		MIDI CC 94,	0...127
--
-- fe attack:		MIDI CC 95,	0...127
-- fe decay:		MIDI CC 96,	0...127
-- fe sustain:		MIDI CC 97,	0...127
-- fe decay 2:		MIDI CC 98,	0...127
-- fe sustain 2:	MIDI CC 99,	0...127
-- fe release:		MIDI CC 100,	0...127
--
-- ae attack:		MIDI CC 101,	0...127
-- ae decay:		MIDI CC 102,	0...127
-- ae sustain:		MIDI CC 103,	0...127
-- ae decay 2:		MIDI CC 104,	0...127
-- ae sustain 2:	MIDI CC 105,	0...127
-- ae release:		MIDI CC 106,	0...127
--
-- e3 attack:		MIDI CC 107,	0...127
-- e3 decay:		MIDI CC 108,	0...127
-- e3 sustain:		MIDI CC 109,	0...127
-- e3 decay 2:		MIDI CC 110,	0...127
-- e3 sustain 2:	MIDI CC 111,	0...127
-- e3 release:		MIDI CC 112,	0...127
--
-- e4 attack:		MIDI CC 113,	0...127
-- e4 decay:		MIDI CC 114,	0...127
-- e4 sustain:		MIDI CC 115,	0...127
-- e4 decay 2:		MIDI CC 116,	0...127
-- e4 sustain 2:	MIDI CC 117,	0...127
-- e4 release:		MIDI CC 118,	0...127
--
-- all sounds off:	MIDI CC 120, 	0
-- reset all controls:	MIDI CC 121, 	0
-- local control:	MIDI CC 122,	0...127
-- all notes off:	MIDI CC 123, 	0

(mod_w, mod_w_p) = pF "mod_w" (Just 0)
(br_ctrl, br_ctrl_p) = pF "br_ctrl" (Just 0)
(ft_ctrl, ft_ctrl_p) = pF "ft_ctrl" (Just 0)
(gl_rate, gl_rate_p) = pF "gl_rate" (Just 0)
(ch_vol, ch_vol_p) = pF "ch_vol" (Just 0)
(pan_, pan__p) = pF "pan" (Just 0)
(arp_rng, arp_rng_p) = pF "arp_rng" (Just 0)
(arp_len, arp_len_p) = pF "arp_len" (Just 0)
(arp_act, arp_act_p) = pF "arp_act" (Just 0)
(lfo1shape, lfo1shape_p) = pF "lfo1shape" (Just 0)
(lfo1speed, lfo1speed_p) = pF "lfo1speed" (Just 0)
(lfo1sync, lfo1sync_p) = pF "lfo1sync" (Just 0)
(lfo1delay, lfo1delay_p) = pF "lfo1delay" (Just 0)
(lfo2shape, lfo2shape_p) = pF "lfo2shape" (Just 0)
(lfo2speed, lfo2speed_p) = pF "lfo2speed" (Just 0)
(lfo2sync, lfo2sync_p) = pF "lfo2sync" (Just 0)
(lfo2delay, lfo2delay_p) = pF "lfo2delay" (Just 0)
(lfo3shape, lfo3shape_p) = pF "lfo3shape" (Just 0)
(lfo3speed, lfo3speed_p) = pF "lfo3speed" (Just 0)
(lfo3sync, lfo3sync_p) = pF "lfo3sync" (Just 0)
(lfo3delay, lfo3delay_p) = pF "lfo3delay" (Just 0)
(osc1oct, osc1oct_p) = pF "osc1oct" (Just 0)
(osc1semi, osc1semi_p) = pF "osc1semi" (Just 0)
(osc1detune, osc1detune_p) = pF "osc1detune" (Just 0)
(osc1fm, osc1fm_p) = pF "osc1fm" (Just 0)
(osc1shape, osc1shape_p) = pF "osc1shape" (Just 0)
(bank_sel, bank_sel_p) = pF "bank_sel" (Just 0)
(osc1pw, osc1pw_p) = pF "osc1pw" (Just 0)
(osc1pwm, osc1pwm_p) = pF "osc1pwm" (Just 0)
(osc2oct, osc2oct_p) = pF "osc2oct" (Just 0)
(osc2semi, osc2semi_p) = pF "osc2semi" (Just 0)
(osc2detune, osc2detune_p) = pF "osc2detune" (Just 0)
(osc2fm, osc2fm_p) = pF "osc2fm" (Just 0)
(osc2shape, osc2shape_p) = pF "osc2shape" (Just 0)
(osc2pw, osc2pw_p) = pF "osc2pw" (Just 0)
(osc2pwm, osc2pwm_p) = pF "osc2pwm" (Just 0)
(osc3oct, osc3oct_p) = pF "osc3oct" (Just 0)
(osc3semi, osc3semi_p) = pF "osc3semi" (Just 0)
(osc3detune, osc3detune_p) = pF "osc3detune" (Just 0)
(osc3fm, osc3fm_p) = pF "osc3fm" (Just 0)
(osc3shape, osc3shape_p) = pF "osc3shape" (Just 0)
(osc3pw, osc3pw_p) = pF "osc3pw" (Just 0)
(osc3pwm, osc3pwm_p) = pF "osc3pwm" (Just 0)
(sync, sync_p) = pF "sync" (Just 0)
(pitchmod, pitchmod_p) = pF "pitchmod" (Just 0)
(glide_mode, glide_mode_p) = pF "glide_mode" (Just 0)
(osc1lvl, osc1lvl_p) = pF "osc1lvl" (Just 0)
(osc1bal, osc1bal_p) = pF "osc1bal" (Just 0)
(ringmod_lvl, ringmod_lvl_p) = pF "ringmod_lvl" (Just 0)
(ringmod_bal, ringmod_bal_p) = pF "ringmod_bal" (Just 0)
(osc2lvl, osc2lvl_p) = pF "osc2lvl" (Just 0)
(osc2bal, osc2bal_p) = pF "osc2bal" (Just 0)
(osc3lvl, osc3lvl_p) = pF "osc3lvl" (Just 0)
(osc3bal, osc3bal_p) = pF "osc3bal" (Just 0)
(noise_lvl, noise_lvl_p) = pF "noise_lvl" (Just 0)
(noise_bal, noise_bal_p) = pF "noise_bal" (Just 0)
(noise_col, noise_col_p) = pF "noise_col" (Just 0)
(sus_ped, sus_ped_p) = pF "sus_ped" (Just 0)
(glide_act, glide_act_p) = pF "glide_act" (Just 0)
(sostenuto, sostenuto_p) = pF "sostenuto" (Just 0)
(routing, routing_p) = pF "routing" (Just 0)
(fil1tp, fil1tp_p) = pF "fil1tp" (Just 0)
(fil1cut, fil1cut_p) = pF "fil1cut" (Just 0)
(fil1res, fil1res_p) = pF "fil1res" (Just 0)
(fil1drv, fil1drv_p) = pF "fil1drv" (Just 0)
(fil1key, fil1key_p) = pF "fil1key" (Just 0)
(fil1enva, fil1enva_p) = pF "fil1enva" (Just 0)
(fil1envv, fil1envv_p) = pF "fil1envv" (Just 0)
(fil1cutmo, fil1cutmo_p) = pF "fil1cutmo" (Just 0)
(fil1fm, fil1fm_p) = pF "fil1fm" (Just 0)
(fil1pan, fil1pan_p) = pF "fil1pan" (Just 0)
(fil1panmod, fil1panmod_p) = pF "fil1panmod" (Just 0)
(fil2tp, fil2tp_p) = pF "fil2tp" (Just 0)
(fil2cut, fil2cut_p) = pF "fil2cut" (Just 0)
(fil2res, fil2res_p) = pF "fil2res" (Just 0)
(fil2drv, fil2drv_p) = pF "fil2drv" (Just 0)
(fil2key, fil2key_p) = pF "fil2key" (Just 0)
(fil2enva, fil2enva_p) = pF "fil2enva" (Just 0)
(fil2envv, fil2envv_p) = pF "fil2envv" (Just 0)
(fil2cutmo, fil2cutmo_p) = pF "fil2cutmo" (Just 0)
(fil2fm, fil2fm_p) = pF "fil2fm" (Just 0)
(fil2pan, fil2pan_p) = pF "fil2pan" (Just 0)
(fil2panmod, fil2panmod_p) = pF "fil2panmod" (Just 0)
(amp_vol, amp_vol_p) = pF "amp_vol" (Just 0)
(amp_vel, amp_vel_p) = pF "amp_vel" (Just 0)
(amp_mod, amp_mod_p) = pF "amp_mod" (Just 0)
(fx1mix, fx1mix_p) = pF "fx1mix" (Just 0)
(fx2mix, fx2mix_p) = pF "fx2mix" (Just 0)
(fe_att, fe_att_p) = pF "fe_att" (Just 0)
(fe_dec, fe_dec_p) = pF "fe_dec" (Just 0)
(fe_sus, fe_sus_p) = pF "fe_sus" (Just 0)
(fe_dec2, fe_dec2_p) = pF "fe_dec2" (Just 0)
(fe_sus2, fe_sus2_p) = pF "fe_sus2" (Just 0)
(fe_rel, fe_rel_p) = pF "fe_rel" (Just 0)
(ae_att, ae_att_p) = pF "ae_att" (Just 0)
(ae_dec, ae_dec_p) = pF "ae_dec" (Just 0)
(ae_sus, ae_sus_p) = pF "ae_sus" (Just 0)
(ae_dec2, ae_dec2_p) = pF "ae_dec2" (Just 0)
(ae_sus2, ae_sus2_p) = pF "ae_sus2" (Just 0)
(ae_rel, ae_rel_p) = pF "ae_rel" (Just 0)
(e3_att, e3_att_p) = pF "e3_att" (Just 0)
(e3_dec, e3_dec_p) = pF "e3_dec" (Just 0)
(e3_sus, e3_sus_p) = pF "e3_sus" (Just 0)
(e3_dec2, e3_dec2_p) = pF "e3_dec2" (Just 0)
(e3_sus2, e3_sus2_p) = pF "e3_sus2" (Just 0)
(e3_rel, e3_rel_p) = pF "e3_rel" (Just 0)
(e4_att, e4_att_p) = pF "e4_att" (Just 0)
(e4_dec, e4_dec_p) = pF "e4_dec" (Just 0)
(e4_sus, e4_sus_p) = pF "e4_sus" (Just 0)
(e4_dec2, e4_dec2_p) = pF "e4_dec2" (Just 0)
(e4_sus2, e4_sus2_p) = pF "e4_sus2" (Just 0)
(e4_rel, e4_rel_p) = pF "e4_rel" (Just 0)
(soff, soff_p) = pF "soff" (Just 0)
(res_ctrl, res_ctrl_p) = pF "res_ctrl" (Just 0)
(loc_cont, loc_cont_p) = pF "loc_cont" (Just 0)
(noff, noff_p) = pF "noff" (Just 0)


blofeldController :: ControllerShape
blofeldController = ControllerShape {
  controls = [
	mCC mod_w_p 1,
	mCC br_ctrl_p 2,
	mCC ft_ctrl_p 4,
	mCC gl_rate_p 5,
	mCC ch_vol_p 7,
	mCC pan__p 10,
	mCC arp_rng_p 12,
	mCC arp_len_p 13,
	mCC arp_act_p 14,
	CC lfo1shape_p 15 (0, 5) passThru -- 0..5 - sine,triangle,square,saw,random,sample&hold
	,mCC lfo1speed_p 16,
	CC lfo1sync_p 17 (0, 1) passThru -- 0 off, 1 on
	,mCC lfo1delay_p 18,
	mCC lfo2shape_p 19,
	mCC lfo2speed_p 20,
	mCC lfo2sync_p 21,
	mCC lfo2delay_p 22,
	mCC lfo3shape_p 23,
	mCC lfo3speed_p 24,
	mCC lfo3sync_p 25,
	mCC lfo3delay_p 26,
	CC osc1oct_p 27 (16, 112) passThru -- 16, 28, 40 .. 112 - 128' .. 1/2'
	,CC osc1semi_p 28 (52, 76) passThru -- 52 .. 76 - -12 - +12 semitones
	,mCC osc1detune_p 29,
	mCC osc1fm_p 30,
	CC osc1shape_p 31 (0, 5) passThru -- 0..5 - pulse, saw, tri, sine, alt 1, alt 2
	,mCC bank_sel_p 32,
	mCC osc1pw_p 33,
	mCC osc1pwm_p 34,
	mCC osc2oct_p 35,
	mCC osc2semi_p 36,
	mCC osc2detune_p 37,
	mCC osc2fm_p 38,
	mCC osc2shape_p 39,
	mCC osc2pw_p 40,
	mCC osc2pwm_p 41,
	mCC osc3oct_p 42,
	mCC osc3semi_p 43,
	mCC osc3detune_p 44,
	mCC osc3fm_p 45,
	mCC osc3shape_p 46,
	mCC osc3pw_p 47,
	mCC osc3pwm_p 48,
	mCC sync_p 49,
	mCC pitchmod_p 50,
	mCC glide_mode_p 51,
	mCC osc1lvl_p 52,
	mCC osc1bal_p 53,
	mCC ringmod_lvl_p 54,
	mCC ringmod_bal_p 55,
	mCC osc2lvl_p 56,
	mCC osc2bal_p 57,
	mCC osc3lvl_p 58,
	mCC osc3bal_p 59,
	mCC noise_lvl_p 60,
	mCC noise_bal_p 61,
	mCC noise_col_p 62,
	mCC sus_ped_p 64,
	mCC glide_act_p 65,
	mCC sostenuto_p 66,
	mCC routing_p 67,
	mCC fil1tp_p 68,
	mCC fil1cut_p 69,
	mCC fil1res_p 70,
	mCC fil1drv_p 71,
	mCC fil1key_p 72,
	mCC fil1enva_p 73,
	mCC fil1envv_p 74,
	mCC fil1cutmo_p 75,
	mCC fil1fm_p 76,
	mCC fil1pan_p 77,
	mCC fil1panmod_p 78,
	mCC fil2tp_p 79,
	mCC fil2cut_p 80,
	mCC fil2res_p 81,
	mCC fil2drv_p 82,
	mCC fil2key_p 83,
	mCC fil2enva_p 84,
	mCC fil2envv_p 85,
	mCC fil2cutmo_p 86,
	mCC fil2fm_p 87,
	mCC fil2pan_p 88,
	mCC fil2panmod_p 89,
	mCC amp_vol_p 90,
	mCC amp_vel_p 91,
	mCC amp_mod_p 92,
	mCC fx1mix_p 93,
	mCC fx2mix_p 94,
	mCC fe_att_p 95,
	mCC fe_dec_p 96,
	mCC fe_sus_p 97,
	mCC fe_dec2_p 98,
	mCC fe_sus2_p 99,
	mCC fe_rel_p 100,
	mCC ae_att_p 101,
	mCC ae_dec_p 102,
	mCC ae_sus_p 103,
	mCC ae_dec2_p 104,
	mCC ae_sus2_p 105,
	mCC ae_rel_p 106,
	mCC e3_att_p 107,
	mCC e3_dec_p 108,
	mCC e3_sus_p 109,
	mCC e3_dec2_p 110,
	mCC e3_sus2_p 111,
	mCC e3_rel_p 112,
	mCC e4_att_p 113,
	mCC e4_dec_p 114,
	mCC e4_sus_p 115,
	mCC e4_dec2_p 116,
	mCC e4_sus2_p 117,
	mCC e4_rel_p 118,
	mCC soff_p 120,
	mCC res_ctrl_p 121,
	mCC loc_cont_p 122,
	mCC noff_p 123
  ],
  latency = 0.1 }

blofeld = toShape blofeldController
