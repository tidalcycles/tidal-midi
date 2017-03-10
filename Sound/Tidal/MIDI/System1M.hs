module Sound.Tidal.MIDI.System1M where

import Sound.Tidal.MIDI.Control
import Sound.Tidal.Params

system1mController :: ControllerShape
system1mController =
  ControllerShape
  { controls =
      [ mCC rmod_p 1
      , mCC rlpcutoff_p 3
      , mCC rport_p 5
      , mCC rres_p 9
      , mCC rcrush_p 12
      , mCC rdelaytime_p 13
      , mCC rosc1_p 16
      , mCC rosc2_p 17
      , mCC rsub_p 18
      , mCC rnoise_p 19
      , mCC rpitchenv_p 22
      , mCC rpitchatk_p 23
      , mCC rpitchdecay_p 24
      , mCC rlfopitch_p 26
      , mCC rlfofade_p 27
      , mCC rlfofilter_p 28
      , mCC rlforate_p 29
      , mCC rlfoamp_p 30
      , mCC rlfotype_p 35
      , mCC rosc1type_p 46
      , mCC rosc1range_p 47
      , mCC rosc1color_p 50
      , mCC rosc1xmod_p 52
      , mCC rosc2color_p 55
      , mCC rosc2tune_p 56
      , mCC rosc1mod_p 60
      , mCC rosc2type_p 61
      , mCC rosc2range_p 62
      , mCC rosc2mod_p 63
      , mCC ramptone_p 69
      , mCC rhpcutoff_p 79
      , mCC rfilterenv_p 81
      , mCC rfilterkey_p 82
      , mCC rfilteratk_p 83
      , mCC rfilterdecay_p 84
      , mCC rfiltersustain_p 85
      , mCC rfilterrelease_p 86
      , mCC rampatk_p 89
      , mCC rampdecay_p 90
      , mCC rreverb_p 91
      , mCC rdelay_p 94
      , mCC rampsustain_p 96
      , mCC ramprelease_p 97
      , mCC rosc2ring_p 111
      , mCC rosc2sync_p 112
      , mCC rsubtype_p 113
      , mCC rnoisetype_p 114
      , mCC rlpftype_p 115
      , mCC rlegato_p 116
      , mCC rlfokeytrig_p 117
      , mCC rtemposync_p 118
      , mCC rmono_p 119
      ]
  , latency = 0.1
  }

system1m = toShape system1mController

(rmod, rmod_p) = pF "rmod" (Just 0)

(rlpcutoff, rlpcutoff_p) = pF "rlpcutoff" (Just 0)

(rport, rport_p) = pF "rport" (Just 0)

(rres, rres_p) = pF "rres" (Just 0)

(rcrush, rcrush_p) = pF "rcrush" (Just 0)

(rdelaytime, rdelaytime_p) = pF "rdelaytime" (Just 0)

(rosc1, rosc1_p) = pF "rosc1" (Just 0)

(rosc2, rosc2_p) = pF "rosc2" (Just 0)

(rsub, rsub_p) = pF "rsub" (Just 0)

(rnoise, rnoise_p) = pF "rnoise" (Just 0)

(rpitchenv, rpitchenv_p) = pF "rpitchenv" (Just 0)

(rpitchatk, rpitchatk_p) = pF "rpitchatk" (Just 0)

(rpitchdecay, rpitchdecay_p) = pF "rpitchdecay" (Just 0)

(rlfopitch, rlfopitch_p) = pF "rlfopitch" (Just 0)

(rlfofade, rlfofade_p) = pF "rlfofade" (Just 0)

(rlfofilter, rlfofilter_p) = pF "rlfofilter" (Just 0)

(rlforate, rlforate_p) = pF "rlforate" (Just 0)

(rlfoamp, rlfoamp_p) = pF "rlfoamp" (Just 0)

(rlfotype, rlfotype_p) = pF "rlfotype" (Just 0)

(rosc1type, rosc1type_p) = pF "rosc1type" (Just 0)

(rosc1range, rosc1range_p) = pF "rosc1range" (Just 0)

(rosc1color, rosc1color_p) = pF "rosc1color" (Just 0)

(rosc1xmod, rosc1xmod_p) = pF "rosc1xmod" (Just 0)

(rosc2color, rosc2color_p) = pF "rosc2color" (Just 0)

(rosc2tune, rosc2tune_p) = pF "rosc2tune" (Just 0)

(rosc1mod, rosc1mod_p) = pF "rosc1mod" (Just 0)

(rosc2type, rosc2type_p) = pF "rosc2type" (Just 0)

(rosc2range, rosc2range_p) = pF "rosc2range" (Just 0)

(rosc2mod, rosc2mod_p) = pF "rosc2mod" (Just 0)

(ramptone, ramptone_p) = pF "ramptone" (Just 0)

(rhpcutoff, rhpcutoff_p) = pF "rhpcutoff" (Just 0)

(rfilterenv, rfilterenv_p) = pF "rfilterenv" (Just 0)

(rfilterkey, rfilterkey_p) = pF "rfilterkey" (Just 0)

(rfilteratk, rfilteratk_p) = pF "rfilteratk" (Just 0)

(rfilterdecay, rfilterdecay_p) = pF "rfilterdecay" (Just 0)

(rfiltersustain, rfiltersustain_p) = pF "rfiltersustain" (Just 0)

(rfilterrelease, rfilterrelease_p) = pF "rfilterrelease" (Just 0)

(rampatk, rampatk_p) = pF "rampatk" (Just 0)

(rampdecay, rampdecay_p) = pF "rampdecay" (Just 0)

(rreverb, rreverb_p) = pF "rreverb" (Just 0)

(rdelay, rdelay_p) = pF "rdelay" (Just 0)

(rampsustain, rampsustain_p) = pF "rampsustain" (Just 0)

(ramprelease, ramprelease_p) = pF "ramprelease" (Just 0)

(rosc2ring, rosc2ring_p) = pF "rosc2ring" (Just 0)

(rosc2sync, rosc2sync_p) = pF "rosc2sync" (Just 0)

(rsubtype, rsubtype_p) = pF "rsubtype" (Just 0)

(rnoisetype, rnoisetype_p) = pF "rnoisetype" (Just 0)

(rlpftype, rlpftype_p) = pF "rlpftype" (Just 0)

(rlegato, rlegato_p) = pF "rlegato" (Just 0)

(rlfokeytrig, rlfokeytrig_p) = pF "rlfokeytrig" (Just 0)

(rtemposync, rtemposync_p) = pF "rtemposync" (Just 0)

(rmono, rmono_p) = pF "rmono" (Just 0)
