module Sound.Tidal.Tetra where

import Sound.Tidal.Stream (makeI, makeF, (|+|), merge, OscPattern)

import Sound.Tidal.MIDI.Control

import Sound.Tidal.Parse
import Sound.Tidal.Pattern

import Control.Applicative

makeSeqTracks = concat $ map makeSeqTrack [0..3]
  where makeSeqTrack t = map (makeSeqStep t) [0..15]
        makeSeqStep t s = NRPN (makeName t s) (makeCC t s) (0, 127) 0 passThru
        makeName t s = "seq" ++ (show $ t + 1) ++ "step" ++ (show $ s + 1)
        makeCC t s = (t * 16) + (s + 120)

polysynth :: ControllerShape
polysynth = ControllerShape { params = ([
                                  mrNRPN "osc1freq" 0 (0, 120) 0.2,
                                  mrNRPN "osc1detune" 1 (0, 100) 0.5,
                                  NRPN "osc1shape" 2 (0, 103) 1 passThru,
                                  mNRPN "osc1glide" 3,
                                  NRPN "osc1kbd" 4 (0, 1) 1 passThru,

                                  mrNRPN "osc2freq" 5 (0, 120) 0.2,
                                  mrNRPN "osc2detune" 6 (0, 100) 0.5,
                                  NRPN "osc2shape" 7 (0, 103) 1 passThru,
                                  mNRPN "osc2glide" 8,
                                  NRPN "osc2kbd" 9 (0, 1) 1 passThru,

                                  NRPN "oscsync" 10 (0, 1) 0 passThru,
                                  NRPN "glidemode" 11 (0, 3) 0 passThru,
                                  NRPN "oscslop" 12 (0, 5) 0 passThru,
                                  mrNRPN "oscmix" 13 (0, 127) 0.5,

                                  mNRPN "noise" 14,
                                  mrNRPN "kcutoff" 15 (0, 164) 1,
                                  mNRPN "kresonance" 16,
                                  mNRPN "kamt" 17,
                                  mNRPN "audiomod" 18,
                                  NRPN "fpoles" 19 (0, 1) 1 passThru,

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
                                  mrNRPN "vdcy" 34 (0, 127) 0.5,
                                  mrNRPN "vsus" 35 (0, 127) 0.5,
                                  mNRPN "vrel" 36,

                                  NRPN "lfo1rate" 37 (0, 166) 0 passThru, -- unsynced
                                  -- mrNRPN "lfo1step" 37 (151, 166) 0, -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo1shape" 38 (0, 4) 0 passThru,
                                  mNRPN "lfo1amt" 39,
                                  NRPN "lfo1dest" 40 (0, 43) 0 passThru,
                                  NRPN "lfo1sync" 41 (0, 1) 0 passThru,

                                  NRPN "lfo2rate" 42 (0, 166) 0 passThru, -- unsynced
                                  -- mrNRPN "lfo2step" 42 (151, 166) 0, -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo2shape" 43 (0, 4) 0 passThru,
                                  mNRPN "lfo2amt" 44,
                                  NRPN "lfo2dest" 45 (0, 43) 0 passThru,
                                  NRPN "lfo2sync" 46 (0, 1) 0 passThru,

                                  NRPN "lfo3rate" 47 (0, 166) 0 passThru, -- unsynced
                                  -- mrNRPN "lfo3step" 47 (151, 166) 0, -- (bpm / 32) - 16 bpm-cycles
                                  NRPN "lfo3shape" 48 (0, 4) 0 passThru,
                                  mNRPN "lfo3amt" 49,
                                  NRPN "lfo3dest" 50 (0, 43) 0 passThru,
                                  NRPN "lfo3sync" 51 (0, 1) 0 passThru,

                                  NRPN "lfo4rate" 52 (0, 166) 0 passThru, -- unsynced
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

                                  NRPN "seq1dst" 77 (0, 47) 0 passThru,
                                  NRPN "seq2dst" 78 (0, 47) 0 passThru,
                                  NRPN "seq3dst" 79 (0, 47) 0 passThru,
                                  NRPN "seq4dst" 80 (0, 47) 0 passThru,

                                  mrNRPN "mwhl" 81 (0, 254) 0.5,
                                  NRPN "mwhldst" 82 (0, 47) 0 passThru,

                                  mrNRPN "aftt" 83 (0, 254) 0.5,
                                  NRPN "afttdst" 84 (0, 47) 0 passThru,

                                  mrNRPN "breath" 85 (0, 254) 0.5,
                                  NRPN "breathdst" 86 (0, 47) 0 passThru,

                                  mrNRPN "mvel" 87 (0, 254) 0.5,
                                  NRPN "mveldst" 88 (0, 47) 0 passThru,

                                  mrNRPN "foot" 89 (0, 254) 0.5,
                                  NRPN "footdst" 90 (0, 47) 0 passThru,

                                  NRPN "kbpm" 91 (30, 250) 0 passThru,
                                  NRPN "clockdiv" 92 (0, 12) 0 passThru, -- TODO: document values

                                  NRPN "bendrng" 93 (0, 12) 0 passThru,

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
                                  NRPN "kmode" 119 (0, 2) 0 passThru, -- TODO: document values
                                  mCC "notesoff" 123,
                                  mCC "ccreset" 121,
                                  mCC "damp" 64

                          ] ++ makeSeqTracks),
                         duration = ("dur", 0.2),
                         velocity = ("vel", 0.5),
                         latency = 0.04
                       }

oscPolysynth = toOscShape polysynth

note            = makeI oscPolysynth "note"

dur             = makeF oscPolysynth "dur"
vel             = makeF oscPolysynth "vel"
notesoff = makeF oscPolysynth "notesoff"
ccreset = makeF oscPolysynth "ccreset"
damp = makeF oscPolysynth "damp"


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

seq1dst     = makeF oscPolysynth "seq1dst"
seq2dst     = makeF oscPolysynth "seq2dst"
seq3dst     = makeF oscPolysynth "seq3dst"
seq4dst     = makeF oscPolysynth "seq4dst"


mwhl    = makeF oscPolysynth "mwhl"
mwhldst = makeF oscPolysynth "mwhldst"

aftt    = makeF oscPolysynth "aftt"
afttdst = makeF oscPolysynth "afttdst"

breath    = makeF oscPolysynth "breath"
breathdst = makeF oscPolysynth "breathdst"

mvel    = makeF oscPolysynth "mvel"
mveldst = makeF oscPolysynth "mveldst"

foot    = makeF oscPolysynth "foot"
footdst = makeF oscPolysynth "footdst"

bendrng = makeF oscPolysynth "bendrng"

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

-- sequences

seqtrack1 = map (makeF oscPolysynth . (\x -> "seq1step" ++ show x)) [1..16]
seqtrack2 = map (makeF oscPolysynth . (\x -> "seq2step" ++ show x)) [1..16]
seqtrack3 = map (makeF oscPolysynth . (\x -> "seq3step" ++ show x)) [1..16]
seqtrack4 = map (makeF oscPolysynth . (\x -> "seq4step" ++ show x)) [1..16]


seqpatterns p' = map onlyValues events
  where arc' = arc p'
        events = arc' (0,1)
        onlyValues = \(_, _, a) -> p a

sqnp i t p' = foldr (|+|) i (zipWith ($) t patterns)
  where patterns = seqpatterns p'

sqnp1 d p' = sqnp (seq1dst $ p $ show d) seqtrack1 p'
sqnp2 d p' = sqnp (seq2dst $ p $ show d) seqtrack2 p'
sqnp3 d p' = sqnp (seq3dst $ p $ show d) seqtrack3 p'
sqnp4 d p' = sqnp (seq4dst $ p $ show d) seqtrack4 p'


-- abstractions

-- adsr

atk p' = vatk p' |+| fatk p' |+| eatk p'
dcy p' = vdcy p' |+| fdcy p' |+| edcy p'
sus p' = vsus p' |+| fsus p' |+| esus p'
rel p' = vrel p' |+| frel p' |+| erel p'

adsr a d s r = atk a |+| dcy d |+| sus s |+| rel r
asr a d r = atk a |+| dcy d |+| rel r

-- lfo

lfotri = doublePattern 0
lforsaw = doublePattern 1
lfosaw = doublePattern 2
lfopulse = doublePattern 3
lforand = doublePattern 4

lrate r = ((min 150) . (max 0)) <$> p r
lstep s = ((min 166) . (max 151) . (+150)) <$> p s

lfo1 s d r a = lfo1shape s |+| lfo1dest d |+| lfo1rate r |+| lfo1amt a
lfo2 s d r a = lfo2shape s |+| lfo2dest d |+| lfo2rate r |+| lfo2amt a
lfo3 s d r a = lfo3shape s |+| lfo3dest d |+| lfo3rate r |+| lfo3amt a
lfo4 s d r a = lfo4shape s |+| lfo4dest d |+| lfo4rate r |+| lfo4amt a

--lfo = lfo1

-- mod

mod1 s d a = mod1src s |+| mod1dst d |+| mod1amt a
mod2 s d a = mod2src s |+| mod2dst d |+| mod2amt a
mod3 s d a = mod3src s |+| mod3dst d |+| mod3amt a
mod4 s d a = mod4src s |+| mod4dst d |+| mod4amt a

-- mod destination

doublePattern d = (p $ show d) :: Pattern Double

dosc1 = doublePattern 1
dosc2 = doublePattern 2
dosc = doublePattern 3
dmix = doublePattern 4
dnoise = doublePattern 5
dpw1 = doublePattern 6
dpw2 = doublePattern 7
dpw = doublePattern 8
dcut = doublePattern 9
dres = doublePattern 10
damod = doublePattern 11
dvca = doublePattern 12
dspread = doublePattern 13

dlfo1f = doublePattern 14
dlfo2f = doublePattern 15
dlfo3f = doublePattern 16
dlfo4f = doublePattern 17
dlfof = doublePattern 18

dlfo1a = doublePattern 19
dlfo2a = doublePattern 20
dlfo3a = doublePattern 21
dlfo4a = doublePattern 22
dlfoa = doublePattern 23

dfamt = doublePattern 24
dvamt = doublePattern 25
deamt = doublePattern 26
damt = doublePattern 27

dfatk = doublePattern 28
dvatk = doublePattern 29
deatk = doublePattern 30
datk = doublePattern 31

dfdcy = doublePattern 32
dvdcy = doublePattern 33
dedcy = doublePattern 34
ddcy = doublePattern 35

dfrel = doublePattern 36
dvrel = doublePattern 37
derel = doublePattern 38
drel = doublePattern 39

dmod1 = doublePattern 40
dmod2 = doublePattern 41
dmod3 = doublePattern 42
dmod4 = doublePattern 43

dfb = doublePattern 44
dsub1 = doublePattern 45
dsub2 = doublePattern 46
dfbgain = doublePattern 47
dslew = doublePattern 48

-- mod sources

sseq1 = doublePattern 1
sseq2 = doublePattern 2
sseq3 = doublePattern 3
sseq4 = doublePattern 4

slfo1 = doublePattern 5
slfo2 = doublePattern 6
slfo3 = doublePattern 7
slfo4 = doublePattern 8

sfenv = doublePattern 9
svenv = doublePattern 10
seenv = doublePattern 11

spitchb = doublePattern 12
smodwh = doublePattern 13
saftert = doublePattern 14
sbreath = doublePattern 15
sfoot = doublePattern 16
sexpr = doublePattern 17

svel = doublePattern 18
snote = doublePattern 19
snoise = doublePattern 20
-- drums


snare d p' = note p'
  |+| osc1shape zero |+| osc1kbd zero
  |+| osc2shape zero |+| osc2kbd zero
  |+| noise one
  |+| vrel d |+| vsus zero |+| vdcy d
  |+| kcutoff (p "1")
  |+| dur (p "0.01")
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
