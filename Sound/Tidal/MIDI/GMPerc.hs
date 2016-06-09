module Sound.Tidal.MIDI.GMPerc where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

percController :: ControllerShape
percController = ControllerShape {
  controls = [
     mCC balance_p 10,
     mCC reverb_p 91,
     mCC chorus_p 93
     ],
  latency = 0.1
  }

perc = midinote . (percN <$>)

percN :: String -> Int
percN "abd" = 35
percN "bd" = 36
percN "sti" = 37
percN "sn" = 38
percN "cp" = 39
percN "esn" = 40
percN "lft" = 41
percN "ch" = 42
percN "hft" = 43
percN "hh" = 44
percN "lt" = 45
percN "oh" = 46
percN "lmt" = 47
percN "hmt" = 48
percN "cr" = 49
percN "ht" = 50
percN "ri" = 51
percN "cy" = 52
percN "be" = 53
percN "ta" = 54
percN "scy" = 55
percN "cow" = 56
percN "cr2" = 57
percN "vib" = 58
percN "ri2" = 59
percN "hb" = 60
percN "lb" = 61
percN "mhc" = 62
percN "ohc" = 63
percN "lc" = 64
percN "hti" = 65
percN "lti" = 66
percN "hag" = 67
percN "lag" = 68
percN "ca" = 69
percN "ma" = 70
percN "shi" = 71
percN "lhi" = 72
percN "sgui" = 73
percN "lgui" = 74
percN "cl" = 75
percN "hwb" = 76
percN "lwb" = 77
percN "mc" = 78
percN "oc" = 79
percN "mt" = 80
percN "ot" = 81
percN _ = 0

-- general shape for stream
percShape = toShape percController

(balance, balance_p) = pF "balance" (Just 0)
(reverb, reverb_p) = pF "reverb" (Just 0)
(chorus, chorus_p) = pF "chorus" (Just 0)
