module Sound.Tidal.MIDI.Ambika where


import Sound.Tidal.MIDI.Control
import Sound.Tidal.Params
import Sound.Tidal.Stream (makeI, makeF)

ambikaController :: ControllerShape
ambikaController = ControllerShape { controls = [
                            mCC vol_p 7,
                            mCC growl_p 9,
                            mCC fuzz_p 12,
                            mCC crush_p 13,
                            mCC osc1range_p 14,
                            mCC osc1detune_p 15,
                            mCC osc1shape_p 16,
                            mCC osc1param_p 17,
                            mCC osc2shape_p 18,
                            mCC osc2param_p 19,
                            mCC osc2range_p 20,
                            mCC osc2detune_p 21,
                            mCC bal_p 22,
                            mCC xmodtype_p 23,
                            mCC xmodamount_p 24,
                            mCC subshape_p 25,
                            mCC sublevel_p 26,
                            mCC noiselevel_p 27,
                            mCC filtermode_p 28,
                            mCC f2cutoff_p 29,
                            mCC f2res_p 30,
                            mCC f2mode_p 31,
                            mCC lfo1sync_p 44,
                            mCC lfo1rate_p 45,
                            mCC lfo1shape_p 46,
                            mCC lfo4rate_p 47,
                            mCC lfo4shape_p 48,
                            mCC lfo2sync_p 52,
                            mCC lfo2rate_p 53,
                            mCC lfo2shape_p 54,
                            mCC lfo3sync_p 60,
                            mCC lfo3rate_p 61,
                            mCC lfo3shape_p 62,
                            mCC hold_p 64,
                            mCC legato_p 68,
                            mCC env1s_p 70,
                            mCC f1res_p 71,
                            mCC env1r_p 72,
                            mCC env1a_p 73,
                            mCC f1cutoff_p 74,
                            mCC env1d_p 75,
                            mCC env2s_p 78,
                            mCC env2r_p 80,
                            mCC env2a_p 81,
                            mCC env2d_p 83,
                            mCC env3s_p 86,
                            mCC env3r_p 88,
                            mCC env3a_p 89,
                            mCC env3d_p 91,
                            mCC parttuning_p 94,
                            mCC tuningspread_p 95,
                            mCC arpmode_p 102,
                            mCC arpdir_p 103,
                            mCC arpoct_p 104,
                            mCC arppat_p 105,
                            mCC arpres_p 106,
                            mCC polymode_p 107,
                            mCC allsoundsoff_p 120,
                            mCC resetallcontrollers_p 121,
                            mCC allnotesoff_p 123
                          ],
                         --duration = ("dur", 0.05),
                         --velocity = ("vel", 0.5),
                         latency = 0.1
                       }

ambikaSynth = toShape ambikaController

(growl, growl_p) = pF "growl" (Just 0)
(fuzz, fuzz_p) = pF "fuzz" (Just 0)
(acrush, acrush_p) = pF "crush" (Just 0)
(osc1range, osc1range_p) = pF "osc1range" (Just 0)
(osc1detune, osc1detune_p) = pF "osc1detune" (Just 0)
(osc1shape, osc1shape_p) = pF "osc1shape" (Just 0)
(osc1param, osc1param_p) = pF "osc1param" (Just 0)
(osc2range, osc2range_p) = pF "osc2range" (Just 0)
(osc2detune, osc2detune_p) = pF "osc2detune" (Just 0)
(osc2shape, osc2shape_p) = pF "osc2shape" (Just 0)
(osc2param, osc2param_p) = pF "osc2param" (Just 0)
(xmodtype, xmodtype_p) = pF "xmodtype" (Just 0)
(xmodamount, xmodamount_p) = pF "xmodamount" (Just 0)
(subshape, subshape_p) = pF "subshape" (Just 0)
(sublevel, sublevel_p) = pF "sublevel" (Just 0)
(noiselevel, noiselevel_p) = pF "noiselevel" (Just 0)
(filtermode, filtermode_p) = pF "filtermode" (Just 0)
(f1cutoff, f1cutoff_p) = pF "f1cutoff" (Just 0)
(f1res, f1res_p) = pF "f1res" (Just 0)
(f2cutoff, f2cutoff_p) = pF "f2cutoff" (Just 0)
(f2res, f2res_p) = pF "f2res" (Just 0)
(f2mode, f2mode_p) = pF "f2mode" (Just 0)
(lfo1sync, lfo1sync_p) = pF "lfo1sync" (Just 0)
(lfo1rate, lfo1rate_p) = pF "lfo1rate" (Just 0)
(lfo1shape, lfo1shape_p) = pF "lfo1shape" (Just 0)
(lfo2sync, lfo2sync_p) = pF "lfo2sync" (Just 0)
(lfo2rate, lfo2rate_p) = pF "lfo2rate" (Just 0)
(lfo2shape, lfo2shape_p) = pF "lfo2shape" (Just 0)
(lfo3sync, lfo3sync_p) = pF "lfo3sync" (Just 0)
(lfo3rate, lfo3rate_p) = pF "lfo3rate" (Just 0)
(lfo3shape, lfo3shape_p) = pF "lfo3shape" (Just 0)
(lfo4sync, lfo4sync_p) = pF "lfo4sync" (Just 0)
(lfo4rate, lfo4rate_p) = pF "lfo4rate" (Just 0)
(lfo4shape, lfo4shape_p) = pF "lfo4shape" (Just 0)
(env1a, env1a_p) = pF "env1a" (Just 0)
(env1d, env1d_p) = pF "env1d" (Just 0)
(env1s, env1s_p) = pF "env1s" (Just 0)
(env1r, env1r_p) = pF "env1r" (Just 0)
(env2a, env2a_p) = pF "env2a" (Just 0)
(env2d, env2d_p) = pF "env2d" (Just 0)
(env2s, env2s_p) = pF "env2s" (Just 0)
(env2r, env2r_p) = pF "env2r" (Just 0)
(env3a, env3a_p) = pF "env3a" (Just 0)
(env3d, env3d_p) = pF "env3d" (Just 0)
(env3s, env3s_p) = pF "env3s" (Just 0)
(env3r, env3r_p) = pF "env3r" (Just 0)
(arpmode, arpmode_p) = pF "arpmode" (Just 0)
(arpdir, arpdir_p) = pF "arpdir" (Just 0)
(arpoct, arpoct_p) = pF "arpoct" (Just 0)
(arppat, arppat_p) = pF "arppat" (Just 0)
(arpres, arpres_p) = pF "arpres" (Just 0)
(parttuning, parttuning_p) = pF "parttuning" (Just 0)
(tuningspread,tuningspread_p) = pF "tuningspread" (Just 0)
--(hold, hold_p) = pF "hold" (Just 0)
--(legato, legato_p) = pF "legato" (Just 0)
(polymode, polymode_p) = pF "polymode" (Just 0)
(allsoundsoff, allsoundsoff_p) = pF "allsoundsoff" (Just 0)
(allnotesoff, allnotesoff_p) = pF "allnotesoff" (Just 0)
(resetallcontrollers, resetallcontrollers_p) = pF "resetallcontrollers" (Just 0)
(bal, bal_p) = pF "bal" (Just 0)
(vol, vol_p) = pF "vol" (Just 0)
