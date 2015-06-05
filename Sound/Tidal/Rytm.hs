module Sound.Tidal.Rytm where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape { params = [

                            mCC "synth1" 16,
                            mCC "synth2" 17,
                            mCC "synth3" 18,
                            mCC "synth4" 19,
                            mCC "synth5" 20,
                            mCC "synth6" 21,
                            mCC "synth7" 22,
                            mCC "synth8" 23,

                            mCC "revpre" 24,
                            mCC "revtime" 25,
                            mCC "revfrq" 26,
                            mCC "revgain" 27,
                            mCC "revhpf" 28,
                            mCC "revlpf" 29,
                            mCC "revvol" 31,

                            mCC "samptune" 24,
                            mCC "sampfinetune" 25,
                            mCC "sampbitreduction" 26,
                            mCC "sampslot" 27,
                            mCC "sampstart" 28,
                            mCC "sampend" 29,
                            mCC "samploop" 30,
                            mCC "samplevel" 31,

                            mCC "machtype" 15,

                            mCC "filtatk" 70,
                            mCC "filtdec" 71,
                            mCC "filtsus" 72,
                            mCC "filtrel" 73,
                            mCC "filtfrq" 74,
                            mCC "filtres" 75,
                            mCC "filttyp" 76,
                            mCC "filtenv" 77,

                            mCC "perf1" 35,
                            mCC "perf2" 36,
                            mCC "perf3" 37,
                            mCC "perf4" 39,
                            mCC "perf5" 40,
                            mCC "perf6" 41,
                            mCC "perf7" 42,
                            mCC "perf8" 43,

                            mCC "atk" 78,
                            mCC "hld" 79,
                            mCC "dec" 80,
                            mCC "ovr" 81,
                            mCC "del" 82,
                            mCC "amprev" 83,
                            mCC "amppan" 10,
                            mCC "vol" 7
                          ],
                         duration = ("dur", 0.05),
                         velocity = ("vel", 0.5),
                         latency = 0.1
                       }

oscKeys = toOscShape keys

-- note on/off
note         = makeI oscKeys "note"
dur          = makeF oscKeys "dur"

-- standard synth params
synth1       = makeF oscKeys "synth1"
synth2       = makeF oscKeys "synth2"
synth3       = makeF oscKeys "synth3"
synth4       = makeF oscKeys "synth4"
synth5       = makeF oscKeys "synth5"
synth6       = makeF oscKeys "synth6"
synth7       = makeF oscKeys "synth7"
synth8       = makeF oscKeys "synth8"

-- machine type (e.g. hard snare, classic snare, hard bd, classic bd, etc)
machtype     = makeF oscKeys "machtype"

-- generic synth level and tuning
lev          = makeF oscKeys "synth1"
tun          = makeF oscKeys "synth2"

-- generic bd decay and sweep type
bddec        = makeF oscKeys "synth3"
bdswt        = makeF oscKeys "synth5"

-- FM bd params
fmbdfmamt    = makeF oscKeys "synth4"
fmbdfmswt    = makeF oscKeys "synth6"
fmbdfmdec    = makeF oscKeys "synth7"
fmbdfmtun    = makeF oscKeys "synth8"

-- hard bd params
hardbdhold   = makeF oscKeys "synth4"
hardbdsnap   = makeF oscKeys "synth6"
hardbdwav    = makeF oscKeys "synth7"
hardbdtic    = makeF oscKeys "synth8"

-- classic bd params
clasbdswd    = makeF oscKeys "synth6"
clasbdtra    = makeF oscKeys "synth7"
clasbdwav    = makeF oscKeys "synth8"

-- generic sd params (decay, noise decay, noise level)
sddec        = makeF oscKeys "synth3"
sdnod        = makeF oscKeys "synth6"
sdnol        = makeF oscKeys "synth7"

-- hard sd params
hardsdswd    = makeF oscKeys "synth4"
hardsdtic    = makeF oscKeys "synth5"
hardsdswt    = makeF oscKeys "synth8"

-- classic sd params
classddet    = makeF oscKeys "synth4"
classdsnp    = makeF oscKeys "synth5"
classdbal    = makeF oscKeys "synth8"

-- FM sd params
fmsdfmt      = makeF oscKeys "synth4"
fmsdfmd      = makeF oscKeys "synth5"
fmsdfma      = makeF oscKeys "synth8"

-- BT
btdec        = makeF oscKeys "synth3"

-- CP
cpton        = makeF oscKeys "synth2"
cpnod        = makeF oscKeys "synth3"
cpnum        = makeF oscKeys "synth4"
cprat        = makeF oscKeys "synth5"
cpnol        = makeF oscKeys "synth6"
cprnd        = makeF oscKeys "synth7"
cpcpd        = makeF oscKeys "synth8"

-- filter params
filtatk      = makeF oscKeys "filtatk"
filtdec      = makeF oscKeys "filtdec"
filtsus      = makeF oscKeys "filtsus"
filtrel      = makeF oscKeys "filtrel"
filtfrq      = makeF oscKeys "filtfrq"
filtres      = makeF oscKeys "filtres"
filttyp      = makeF oscKeys "filttyp"
filtenv      = makeF oscKeys "filtenv"

-- amplitude params
atk          = makeF oscKeys "atk"
hld          = makeF oscKeys "hld"
dec          = makeF oscKeys "dec"
ovr          = makeF oscKeys "ovr"
del          = makeF oscKeys "del"
amprev       = makeF oscKeys "amprev"
amppan       = makeF oscKeys "amppan"
vol          = makeF oscKeys "vol"

-- delay params (only used on FX MIDI channel)
deltime      = makeF oscKeys "synth1"
delpingpong  = makeF oscKeys "synth2"
delwidth     = makeF oscKeys "synth3"
delfeedback  = makeF oscKeys "synth4"
delhpf       = makeF oscKeys "synth5"
dellpf       = makeF oscKeys "synth6"
delrev       = makeF oscKeys "synth7"
delvol       = makeF oscKeys "synth8"

-- reverb params (only used on FX MIDI channel)
revpre       = makeF oscKeys "revpre"
revtime      = makeF oscKeys "revtime"
revfrq       = makeF oscKeys "revfrq"
revgain      = makeF oscKeys "revgain"
revhpf       = makeF oscKeys "revhpf"
revlpf       = makeF oscKeys "revlpf"
revvol       = makeF oscKeys "revvol"

-- performance
perf1        = makeF oscKeys "perf1"
perf2        = makeF oscKeys "perf2"
perf3        = makeF oscKeys "perf3"
perf4        = makeF oscKeys "perf4"
perf5        = makeF oscKeys "perf5"
perf6        = makeF oscKeys "perf6"
perf7        = makeF oscKeys "perf7"
perf8        = makeF oscKeys "perf8"

-- sample
samptune         = makeF oscKeys "samptune"
sampfinetune     = makeF oscKeys "sampfinetune"
sampbitreduction = makeF oscKeys "sampbitreduction"
sampslot         = makeF oscKeys "sampslot"
sampstart        = makeF oscKeys "sampstart"
sampend          = makeF oscKeys "sampend"
samploop         = makeF oscKeys "samploop"
samplevel        = makeF oscKeys "samplevel"
