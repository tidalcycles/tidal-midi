module Sound.Tidal.Tetra where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

import Sound.Tidal.Parse


polysynth :: ControllerShape
polysynth = ControllerShape { params = [
                                  NRPN "osc1freq" 0 (0, 120),
                                  NRPN "osc1detune" 1 (0, 100),
                                  NRPN "osc1shape" 2 (0, 103),
                                  NRPN "osc1glide" 3 (0, 127),
                                  NRPN "osc1kbd" 4 (0, 1),

                                  NRPN "osc2freq" 5 (0, 120),
                                  NRPN "osc2detune" 6 (0, 100),
                                  NRPN "osc2shape" 7 (0, 103),
                                  NRPN "osc2glide" 8 (0, 127),
                                  NRPN "osc2kbd" 9 (0, 1),

                                  NRPN "oscsync" 10 (0, 1),
                                  NRPN "glidemode" 11 (0, 3),
                                  NRPN "oscslop" 12 (0, 5),
                                  NRPN "oscmix" 13 (0, 127),

                                  NRPN "noise" 14 (0, 127),
                                  NRPN "kcutoff" 15 (0, 164),
                                  NRPN "kresonance" 16 (0, 127),
                                  NRPN "kamt" 17 (0, 127),
                                  NRPN "audiomod" 18 (0, 127),
                                  NRPN "fpoles" 19 (0, 1),

                                  -- filter envelope
                                  NRPN "famt" 20 (0, 254),
                                  NRPN "fvel" 21 (0, 127),
                                  NRPN "fdel" 22 (0, 127),
                                  NRPN "fatk" 23 (0, 127),
                                  NRPN "fdcy" 24 (0, 127),
                                  NRPN "fsus" 25 (0, 127),
                                  NRPN "frel" 26 (0, 127),


                                  NRPN "vcavol" 27 (0, 127),
                                  NRPN "outspread" 28 (0, 127),
                                  NRPN "vol" 29 (0, 127),

                                  NRPN "vamt" 30 (0, 127),
                                  NRPN "vvel" 31 (0, 127),
                                  NRPN "vdel" 32 (0, 127),
                                  NRPN "vatk" 33 (0, 127),
                                  NRPN "vdcy" 34 (0, 127),
                                  NRPN "vsus" 35 (0, 127),
                                  NRPN "vrel" 36 (0, 127),

                                  NRPN "lfo1rate" 37 (0, 150), -- unsynced
                                  NRPN "lfo1step" 37 (151, 166), -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo1shape" 38 (0, 4),
                                  NRPN "lfo1amt" 39 (0, 127),
                                  NRPN "lfo1dest" 40 (0, 43),
                                  NRPN "lfo1sync" 41 (0, 1),

                                  NRPN "lfo2rate" 42 (0, 150), -- unsynced
                                  NRPN "lfo2step" 42 (151, 166), -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo2shape" 43 (0, 4),
                                  NRPN "lfo2amt" 44 (0, 127),
                                  NRPN "lfo2dest" 45 (0, 43),
                                  NRPN "lfo2sync" 46 (0, 1),

                                  NRPN "lfo3rate" 47 (0, 150), -- unsynced
                                  NRPN "lfo3step" 47 (151, 166), -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo3shape" 48 (0, 4),
                                  NRPN "lfo3amt" 49 (0, 127),
                                  NRPN "lfo3dest" 50 (0, 43),
                                  NRPN "lfo3sync" 51 (0, 1),

                                  NRPN "lfo4rate" 52 (0, 150), -- unsynced
                                  NRPN "lfo4step" 52 (151, 166), -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo4shape" 53 (0, 4),
                                  NRPN "lfo4amt" 54 (0, 127),
                                  NRPN "lfo4dest" 55 (0, 43),
                                  NRPN "lfo4sync" 56 (0, 1),

                                  NRPN "emod" 57 (0, 43),
                                  NRPN "eamt" 58 (0, 254),
                                  NRPN "evel" 59 (0, 127),
                                  NRPN "edel" 60 (0, 127),
                                  NRPN "eatk" 61 (0, 127),
                                  NRPN "edcy" 62 (0, 127),
                                  NRPN "esus" 63 (0, 127),
                                  NRPN "erel" 64 (0, 127),

                                  NRPN "mod1src" 65 (0, 20),
                                  NRPN "mod1amt" 66 (0, 254),
                                  NRPN "mod1dst" 67 (0, 47),

                                  NRPN "mod2src" 68 (0, 20),
                                  NRPN "mod2amt" 69 (0, 254),
                                  NRPN "mod2dst" 70 (0, 47),

                                  NRPN "mod3src" 71 (0, 20),
                                  NRPN "mod3amt" 72 (0, 254),
                                  NRPN "mod3dst" 73 (0, 47),

                                  NRPN "mod4src" 74 (0, 20),
                                  NRPN "mod4amt" 75 (0, 254),
                                  NRPN "mod4dst" 76 (0, 47),

                                  -- left out: modwheel, breath, footctrl, pressure, velocity

                                  NRPN "kbpm" 91 (30, 250),
                                  NRPN "clockdiv" 92 (0, 12),

                                  -- left out: pitchbend range

                                  NRPN "sqntrig" 94 (0, 4),
                                  NRPN "unisonkey" 95 (0, 5),
                                  NRPN "unisonmode" 96 (0, 4),
                                  NRPN "arpmode" 97 (0, 14),

                                  NRPN "erepeat" 98 (0, 1),
                                  NRPN "unison" 99 (0, 1),


                                  NRPN "arp" 100 (0, 1),
                                  NRPN "sqn" 101 (0, 1),

                                  NRPN "mcr1" 105 (0, 183),
                                  NRPN "mcr2" 106 (0, 183),
                                  NRPN "mcr3" 107 (0, 183),
                                  NRPN "mcr4" 108 (0, 183),

                                  NRPN "fbgain" 110 (0, 127),

                                  NRPN "btnfreq" 111 (0, 127),
                                  NRPN "btnvel" 112 (0, 127),
                                  NRPN "btnmode" 113 (0, 1),

                                  NRPN "sub1vol" 114 (0, 127),
                                  NRPN "sub2vol" 115 (0, 127),

                                  NRPN "fbvol" 116 (0, 127),

                                  -- left out: editor byte,

                                  NRPN "ksplitpoint" 118 (0, 127),
                                  NRPN "kmode" 119 (0, 2)
                          ],
                         duration = ("dur", 0.05),
                         latency = 0.04
                       }

oscPolysynth = toOscShape polysynth

note            = makeI oscPolysynth "note"

dur             = makeF oscPolysynth "dur"


osc1freq      = makeF oscPolysynth "osc1freq"
osc1detune      = makeF oscPolysynth "osc1detune"
osc1shape     = makeF oscPolysynth "osc1shape"
osc1glide     = makeF oscPolysynth "osc1glide"
osc1kbd     = makeF oscPolysynth "osc1kbd"

osc2freq      = makeF oscPolysynth "osc2freq"
osc2detune      = makeF oscPolysynth "osc2detune"
osc2shape     = makeF oscPolysynth "osc2shape"
osc2glide     = makeF oscPolysynth "osc2glide"
osc2kbd     = makeF oscPolysynth "osc2kbd"

oscsync     = makeF oscPolysynth "oscsync"
glidemode     = makeF oscPolysynth "glidemode"
oscslop     = makeF oscPolysynth "oscslop"
oscmix      = makeF oscPolysynth "oscmix"

noise     = makeF oscPolysynth "noise"
kcutoff     = makeF oscPolysynth "kcutoff"
kresonance      = makeF oscPolysynth "kresonance"
kamt      = makeF oscPolysynth "kamt"
audiomod      = makeF oscPolysynth "audiomod"
fpoles      = makeF oscPolysynth "fpoles"
twopole         = fpoles (p "0")
fourpole        = fpoles (p "1")

-- filter envelope
famt      = makeF oscPolysynth "famt"
fvel      = makeF oscPolysynth "fvel"
fdel      = makeF oscPolysynth "fdel"
fatk      = makeF oscPolysynth "fatk"
fdcy      = makeF oscPolysynth "fdcy"
fsus      = makeF oscPolysynth "fsus"
frel      = makeF oscPolysynth "frel"


vcavol      = makeF oscPolysynth "vcavol"
outspread     = makeF oscPolysynth "outspread"
vol     = makeF oscPolysynth "vol"

vamt      = makeF oscPolysynth "vamt"
vvel      = makeF oscPolysynth "vvel"
vdel      = makeF oscPolysynth "vdel"
vatk      = makeF oscPolysynth "vatk"
vdcy      = makeF oscPolysynth "vdcy"
vsus      = makeF oscPolysynth "vsus"
vrel      = makeF oscPolysynth "vrel"

lfo1rate      = makeF oscPolysynth "lfo1rate"
lfo1step      = makeF oscPolysynth "lfo1step"
lfo1shape     = makeF oscPolysynth "lfo1shape"
lfo1amt     = makeF oscPolysynth "lfo1amt"
lfo1dest      = makeF oscPolysynth "lfo1dest"
lfo1sync      = makeF oscPolysynth "lfo1sync"

lfo2rate      = makeF oscPolysynth "lfo2rate"
lfo2step      = makeF oscPolysynth "lfo2step"
lfo2shape     = makeF oscPolysynth "lfo2shape"
lfo2amt     = makeF oscPolysynth "lfo2amt"
lfo2dest      = makeF oscPolysynth "lfo2dest"
lfo2sync      = makeF oscPolysynth "lfo2sync"

lfo3rate      = makeF oscPolysynth "lfo3rate"
lfo3step      = makeF oscPolysynth "lfo3step"
lfo3shape     = makeF oscPolysynth "lfo3shape"
lfo3amt     = makeF oscPolysynth "lfo3amt"
lfo3dest      = makeF oscPolysynth "lfo3dest"
lfo3sync      = makeF oscPolysynth "lfo3sync"

lfo4rate      = makeF oscPolysynth "lfo4rate"
lfo4step      = makeF oscPolysynth "lfo4step"
lfo4shape     = makeF oscPolysynth "lfo4shape"
lfo4amt     = makeF oscPolysynth "lfo4amt"
lfo4dest      = makeF oscPolysynth "lfo4dest"
lfo4sync      = makeF oscPolysynth "lfo4sync"

emod      = makeF oscPolysynth "emod"
eamt      = makeF oscPolysynth "eamt"
evel      = makeF oscPolysynth "evel"
edel      = makeF oscPolysynth "edel"
eatk      = makeF oscPolysynth "eatk"
edcy      = makeF oscPolysynth "edcy"
esus      = makeF oscPolysynth "esus"
erel      = makeF oscPolysynth "erel"

mod1src     = makeF oscPolysynth "mod1src"
mod1amt     = makeF oscPolysynth "mod1amt"
mod1dst     = makeF oscPolysynth "mod1dst"

mod2src     = makeF oscPolysynth "mod2src"
mod2amt     = makeF oscPolysynth "mod2amt"
mod2dst     = makeF oscPolysynth "mod2dst"

mod3src     = makeF oscPolysynth "mod3src"
mod3amt     = makeF oscPolysynth "mod3amt"
mod3dst     = makeF oscPolysynth "mod3dst"

mod4src     = makeF oscPolysynth "mod4src"
mod4amt     = makeF oscPolysynth "mod4amt"
mod4dst     = makeF oscPolysynth "mod4dst"

-- left out: modwheel, breath, footctrl, pressure, velocity

kbpm      = makeF oscPolysynth "kbpm"
clockdiv      = makeF oscPolysynth "clockdiv"

-- left out: pitchbend range

sqntrig     = makeF oscPolysynth "sqntrig"
unisonkey     = makeF oscPolysynth "unisonkey"
unisonmode      = makeF oscPolysynth "unisonmode"
arpmode     = makeF oscPolysynth "arpmode"

erepeat     = makeF oscPolysynth "erepeat"
unison      = makeF oscPolysynth "unison"


arp     = makeF oscPolysynth "arp"
sqn     = makeF oscPolysynth "sqn"

mcr1      = makeF oscPolysynth "mcr1"
mcr2      = makeF oscPolysynth "mcr2"
mcr3      = makeF oscPolysynth "mcr3"
mcr4      = makeF oscPolysynth "mcr4"

fbgain      = makeF oscPolysynth "fbgain"

btnfreq     = makeF oscPolysynth "btnfreq"
btnvel      = makeF oscPolysynth "btnvel"
btnmode     = makeF oscPolysynth "btnmode"

sub1vol     = makeF oscPolysynth "sub1vol"
sub2vol     = makeF oscPolysynth "sub2vol"

fbvol     = makeF oscPolysynth "fbvol"

ksplitpoint     = makeF oscPolysynth "ksplitpoint"
kmode     = makeF oscPolysynth "kmode"
knormal         = kmode (p "0")
kstack          = kmode (p "0.5")
ksplit          = kmode (p "1")
