module Sound.Tidal.MeeblipAnode where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

anode :: ControllerShape
anode = ControllerShape {params = [
                          mCC "modwheel" 1,
                          mCC "attack" 54,
                          mCC "decay" 53,
                          mCC "kcutoff" 52,
                          mCC "pulsewidth" 51,
                          mCC "detune" 50,
                          mCC "lforate" 49,
                          mCC "lfodepth" 48,
                          mCC "portamento" 55,
                          mCC "vcfenvelope" 56,
                          mCC "sustain" 64,
                          mCC "oscboct" 65,
                          mCC "sweep" 66,
                          mCC "lfodest" 67,
                          mCC "lforandom" 68,
                          mCC "lforetrigger" 69,
                          mCC "oscbwave" 70
                        ],
                        duration = ("dur", 0.05),
                        velocity = ("vel", 0.5),
                        latency = 0.1}

oscAnode = toOscShape anode

note          = makeI oscAnode "note"
dur           = makeF oscAnode "dur"
vel           = makeF oscAnode "vel"
modwheel      = makeF oscAnode "modwheel"
attack        = makeF oscAnode "attack"
decay         = makeF oscAnode "decay"
kcutoff       = makeF oscAnode "kcutoff"
pulsewidth    = makeF oscAnode "pulsewidth"
detune        = makeF oscAnode "detune"
lforate       = makeF oscAnode "lforate"
lfodepth      = makeF oscAnode "lfodepth"
portamento    = makeF oscAnode "portamento"
vcfenvelope   = makeF oscAnode "vcfenvelope"
sustain       = makeF oscAnode "sustain"
oscboct       = makeF oscAnode "oscboct"
sweep         = makeF oscAnode "sweep"
lfodest       = makeF oscAnode "lfodest"
lforandom     = makeF oscAnode "lforandom"
lforetrigger  = makeF oscAnode "lforetrigger"
oscbwave      = makeF oscAnode "oscbwave"
