module Sound.Tidal.Tetra where

import Sound.Tidal.Stream (makeI, makeF, (|+|), merge, OscPattern)

import Sound.Tidal.MIDI.Control

import Sound.Tidal.Parse
import Sound.Tidal.Pattern

polysynth :: ControllerShape
polysynth = ControllerShape { params = [
                                  mrNRPN "osc1freq" 0 (0, 120) 0.2,
                                  mrNRPN "osc1detune" 1 (0, 100) 0.5,
                                  NRPN "osc1shape" 2 (0, 103) 0 passThru,
                                  mNRPN "osc1glide" 3,
                                  NRPN "osc1kbd" 4 (0, 1) 0 passThru,

                                  mrNRPN "osc2freq" 5 (0, 120) 0,
                                  mrNRPN "osc2detune" 6 (0, 100) 0.5,
                                  NRPN "osc2shape" 7 (0, 103) 0 passThru,
                                  mNRPN "osc2glide" 8,
                                  NRPN "osc2kbd" 9 (0, 1) 0 passThru,

                                  NRPN "oscsync" 10 (0, 1) 0 passThru,
                                  NRPN "glidemode" 11 (0, 3) 0 passThru,
                                  NRPN "oscslop" 12 (0, 5) 0 passThru,
                                  mrNRPN "oscmix" 13 (0, 127) 0.5,

                                  mNRPN "noise" 14,
                                  mrNRPN "kcutoff" 15 (0, 164) 1,
                                  mNRPN "kresonance" 16,
                                  mNRPN "kamt" 17,
                                  mNRPN "audiomod" 18,
                                  NRPN "fpoles" 19 (0, 1) 0 passThru,

                                  -- filter envelope
                                  mrNRPN "famt" 20 (0, 254) 0.5,
                                  mNRPN "fvel" 21,
                                  mNRPN "fdel" 22,
                                  mNRPN "fatk" 23,
                                  mNRPN "fdcy" 24,
                                  mNRPN "fsus" 25,
                                  mNRPN "frel" 26,


                                  mNRPN "vcavol" 27,
                                  mNRPN "outspread" 28,
                                  mrNRPN "vol" 29 (0, 127) 1, -- max volume by default

                                  mrNRPN "vamt" 30 (0, 127) 1, -- max vca envelope amount
                                  mNRPN "vvel" 31,
                                  mNRPN "vdel" 32,
                                  mNRPN "vatk" 33,
                                  mNRPN "vdcy" 34,
                                  mNRPN "vsus" 35,
                                  mNRPN "vrel" 36,

                                  mrNRPN "lfo1rate" 37 (0, 150) 0, -- unsynced
                                  -- mrNRPN "lfo1step" 37 (151, 166) 0, -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo1shape" 38 (0, 4) 0 passThru,
                                  mNRPN "lfo1amt" 39,
                                  NRPN "lfo1dest" 40 (0, 43) 0 passThru,
                                  NRPN "lfo1sync" 41 (0, 1) 0 passThru,

                                  mrNRPN "lfo2rate" 42 (0, 150) 0, -- unsynced
                                  -- mrNRPN "lfo2step" 42 (151, 166) 0, -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo2shape" 43 (0, 4) 0 passThru,
                                  mNRPN "lfo2amt" 44,
                                  NRPN "lfo2dest" 45 (0, 43) 0 passThru,
                                  NRPN "lfo2sync" 46 (0, 1) 0 passThru,

                                  mrNRPN "lfo3rate" 47 (0, 150) 0 , -- unsynced
                                  -- mrNRPN "lfo3step" 47 (151, 166) 0, -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo3shape" 48 (0, 4) 0 passThru,
                                  mNRPN "lfo3amt" 49,
                                  NRPN "lfo3dest" 50 (0, 43) 0 passThru,
                                  NRPN "lfo3sync" 51 (0, 1) 0 passThru,

                                  mrNRPN "lfo4rate" 52 (0, 150) 0, -- unsynced
                                  -- mrNRPN "lfo4step" 52 (151, 166) 0, -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo4shape" 53 (0, 4) 0 passThru,
                                  mNRPN "lfo4amt" 54,
                                  NRPN "lfo4dest" 55 (0, 43) 0 passThru,
                                  NRPN "lfo4sync" 56 (0, 1) 0 passThru,

                                  NRPN "emod" 57 (0, 43) 0 passThru,
                                  mrNRPN "eamt" 58 (0, 254) 0.5,
                                  mNRPN "evel" 59,
                                  mNRPN "edel" 60,
                                  mNRPN "eatk" 61,
                                  mNRPN "edcy" 62,
                                  mNRPN "esus" 63,
                                  mNRPN "erel" 64,

                                  NRPN "mod1src" 65 (0, 20) 0 passThru,
                                  mrNRPN "mod1amt" 66 (0, 254) 0.5,
                                  NRPN "mod1dst" 67 (0, 47) 0 passThru,

                                  NRPN "mod2src" 68 (0, 20) 0 passThru,
                                  mrNRPN "mod2amt" 69 (0, 254) 0.5,
                                  NRPN "mod2dst" 70 (0, 47) 0 passThru,

                                  NRPN "mod3src" 71 (0, 20) 0 passThru,
                                  mrNRPN "mod3amt" 72 (0, 254) 0.5,
                                  NRPN "mod3dst" 73 (0, 47) 0 passThru,

                                  NRPN "mod4src" 74 (0, 20) 0 passThru,
                                  mrNRPN "mod4amt" 75 (0, 254) 0.5,
                                  NRPN "mod4dst" 76 (0, 47) 0 passThru,

                                  -- left out: modwheel, breath, footctrl, pressure, velocity

                                  NRPN "kbpm" 91 (30, 250) 0 passThru,
                                  NRPN "clockdiv" 92 (0, 12) 0 passThru, -- TODO: document values

                                  -- left out: pitchbend range

                                  NRPN "sqntrig" 94 (0, 4) 0 passThru, -- TODO: document values
                                  NRPN "unisonkey" 95 (0, 5) 0 passThru, -- TODO: document values
                                  NRPN "unisonmode" 96 (0, 4) 0 passThru, -- TODO: document values
                                  NRPN "arpmode" 97 (0, 14) 0 passThru,

                                  NRPN "erepeat" 98 (0, 1) 0 passThru,
                                  NRPN "unison" 99 (0, 1) 0 passThru,


                                  NRPN "arp" 100 (0, 1) 0 passThru,
                                  NRPN "sqn" 101 (0, 1) 0 passThru,

                                  NRPN "mcr1" 105 (0, 183) 0 passThru,
                                  NRPN "mcr2" 106 (0, 183) 0 passThru,
                                  NRPN "mcr3" 107 (0, 183) 0 passThru,
                                  NRPN "mcr4" 108 (0, 183) 0 passThru,

                                  mNRPN "fbgain" 110,

                                  mrNRPN "btnfreq" 111 (0, 127) 0.25,
                                  mrNRPN "btnvel" 112 (0, 127) 1,
                                  NRPN "btnmode" 113 (0, 1) 0 passThru,

                                  mNRPN "sub1vol" 114,
                                  mNRPN "sub2vol" 115,

                                  mNRPN "fbvol" 116,

                                  -- left out: editor byte,

                                  mrNRPN "ksplitpoint" 118 (0, 127) 0.5,
                                  NRPN "kmode" 119 (0, 2) 0 passThru -- TODO: document values
                          ],
                         duration = ("dur", 0.2),
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
-- lfo1step      = makeF oscPolysynth "lfo1step"
lfo1shape     = makeF oscPolysynth "lfo1shape"
lfo1amt     = makeF oscPolysynth "lfo1amt"
lfo1dest      = makeF oscPolysynth "lfo1dest"
lfo1sync      = makeF oscPolysynth "lfo1sync"

lfo2rate      = makeF oscPolysynth "lfo2rate"
-- lfo2step      = makeF oscPolysynth "lfo2step"
lfo2shape     = makeF oscPolysynth "lfo2shape"
lfo2amt     = makeF oscPolysynth "lfo2amt"
lfo2dest      = makeF oscPolysynth "lfo2dest"
lfo2sync      = makeF oscPolysynth "lfo2sync"

lfo3rate      = makeF oscPolysynth "lfo3rate"
-- lfo3step      = makeF oscPolysynth "lfo3step"
lfo3shape     = makeF oscPolysynth "lfo3shape"
lfo3amt     = makeF oscPolysynth "lfo3amt"
lfo3dest      = makeF oscPolysynth "lfo3dest"
lfo3sync      = makeF oscPolysynth "lfo3sync"

lfo4rate      = makeF oscPolysynth "lfo4rate"
-- lfo4step      = makeF oscPolysynth "lfo4step"
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



snare d p' = note p'
  |+| osc1shape zero |+| osc1kbd zero
  |+| osc2shape zero |+| osc2kbd zero
  |+| noise one
  |+| vrel zero |+| vsus zero |+| vdcy d
  |+| kcutoff (p "1")
  |+| dur d
  where zero = p "0"
        one = p "1"


kick blp p' = note p'
  |+| fourpole
  |+| osc1shape zero |+| osc1kbd zero
  |+| osc2shape zero |+| osc2kbd zero
  |+| vsus zero |+| vdcy (p "0.95") |+| vrel (p "0.5")
  |+| kresonance (p "0.99") |+| kcutoff zero
  |+| eamt (p "0.8") |+| emod (p "9") |+| edcy blp
  |+| dur (p "0.2")
  where zero = p "0"
