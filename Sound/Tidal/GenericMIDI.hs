module Sound.Tidal.GenericMIDI where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

genericmidi :: ControllerShape
genericmidi = ControllerShape {params = [

                          -- Parameters according to the midi specification
                          mCC "bankselect" 0,         -- Bank Select (Controller # 32 more commonly used)
                          mCC "modwheel" 1,           -- Modulation Wheel
                          mCC "breath" 2,             -- Breath Contoller
                          mCC "foot" 4,               -- Foot Controller
                          mCC "portamentotime" 5,     -- Portamento Time
                          mCC "volume" 7,             -- Main Volume
                          mCC "balance" 8,            -- Balance
                          mCC "pan" 10,               -- Pan
                          mCC "expression" 11,        -- Expression Controller
                          mCC "effectcontrol1" 12,    -- Effect Control 1
                          mCC "effectcontrol2" 13,    -- Effect Control 2
                          mCC "generalcontroller1" 16,
                          mCC "generalcontroller2" 17,
                          mCC "generalcontroller3" 18,
                          mCC "generalcontroller4" 19,
                          mCC "sustainpedal" 64,      -- Damper Pedal (Sustain) [Data Byte of 0-63=0ff, 64-127=On]
                          mCC "portamento" 65,        -- Portamento
                          mCC "sostenuto" 66,         -- Sostenuto
                          mCC "softpedal" 67,         -- Soft Pedal
                          mCC "legato" 68,            -- Legato Footswitch
                          mCC "hold2" 69,             -- Hold 2
                          mCC "variation" 70,    -- Sound Controller 1 (default: Sound Variation)
                          mCC "vcf" 71,            -- Sound Controller 2 (default: Timbre/Harmonic Content)
                          mCC "vca" 72,           -- Sound Controller 3 (default: Release Time)
                          mCC "attack" 73,            -- Sound Controller 4 (default: Attack Time)
                          mCC "brightness" 74,        -- Sound Controller 5 (default: Brightness)
                          mCC "portamentocontrol" 84, -- Portamento Control
                          mCC "effects1depth" 91,	    -- Effects 1 Depth (previously External Effects Depth)
                          mCC "effects2depth" 92,	    -- Effects 2 Depth (previously Tremolo Depth)
                          mCC "effects3depth" 93,	    -- Effects 3 Depth (previously Chorus Depth)
                          mCC "effects4depth" 94,	    -- Effects 4 Depth (previously Detune Depth)
                          mCC "effects5depth" 95,	    -- Effects 5 Depth (previously Phaser Depth)
                          mCC "reset" 121,            -- Reset All Controllers
                          mCC "localcontrol" 122,     -- Local Control
                          mCC "allnotesoff" 123,      -- All Notes Off
                          mCC "omnioff" 124,          -- Omni Off
                          mCC "omnion" 125,           -- Omni On
                          mCC "monoon" 126,           -- Mono On (Poly Off)
                          mCC "polyon" 127,           -- Poly On (Mono Off)

                          -- numbered
                          mCC "cc0" 0,
                          mCC "cc1" 1,
                          mCC "cc2" 2,
                          mCC "cc3" 3,
                          mCC "cc4" 4,
                          mCC "cc5" 5,
                          mCC "cc6" 6,
                          mCC "cc7" 7,
                          mCC "cc8" 8,
                          mCC "cc9" 9,
                          mCC "cc10" 10,
                          mCC "cc11" 11,
                          mCC "cc12" 12,
                          mCC "cc13" 13,
                          mCC "cc14" 14,
                          mCC "cc15" 15,
                          mCC "cc16" 16,
                          mCC "cc17" 17,
                          mCC "cc18" 18,
                          mCC "cc19" 19,
                          mCC "cc20" 20,
                          mCC "cc21" 21,
                          mCC "cc22" 22,
                          mCC "cc23" 23,
                          mCC "cc24" 24,
                          mCC "cc25" 25,
                          mCC "cc26" 26,
                          mCC "cc27" 27,
                          mCC "cc28" 28,
                          mCC "cc29" 29,
                          mCC "cc30" 30,
                          mCC "cc31" 31,
                          mCC "cc32" 32,
                          mCC "cc33" 33,
                          mCC "cc34" 34,
                          mCC "cc35" 35,
                          mCC "cc36" 36,
                          mCC "cc37" 37,
                          mCC "cc38" 38,
                          mCC "cc39" 39,
                          mCC "cc40" 40,
                          mCC "cc41" 41,
                          mCC "cc42" 42,
                          mCC "cc43" 43,
                          mCC "cc44" 44,
                          mCC "cc45" 45,
                          mCC "cc46" 46,
                          mCC "cc47" 47,
                          mCC "cc48" 48,
                          mCC "cc49" 49,
                          mCC "cc50" 50,
                          mCC "cc51" 51,
                          mCC "cc52" 52,
                          mCC "cc53" 53,
                          mCC "cc54" 54,
                          mCC "cc55" 55,
                          mCC "cc56" 56,
                          mCC "cc57" 57,
                          mCC "cc58" 58,
                          mCC "cc59" 59,
                          mCC "cc60" 60,
                          mCC "cc61" 61,
                          mCC "cc62" 62,
                          mCC "cc63" 63,
                          mCC "cc64" 64,
                          mCC "cc65" 65,
                          mCC "cc66" 66,
                          mCC "cc67" 67,
                          mCC "cc68" 68,
                          mCC "cc69" 69,
                          mCC "cc70" 70,
                          mCC "cc71" 71,
                          mCC "cc72" 72,
                          mCC "cc73" 73,
                          mCC "cc74" 74,
                          mCC "cc75" 75,
                          mCC "cc76" 76,
                          mCC "cc77" 77,
                          mCC "cc78" 78,
                          mCC "cc79" 79,
                          mCC "cc80" 80,
                          mCC "cc81" 81,
                          mCC "cc82" 82,
                          mCC "cc83" 83,
                          mCC "cc84" 84,
                          mCC "cc85" 85,
                          mCC "cc86" 86,
                          mCC "cc87" 87,
                          mCC "cc88" 88,
                          mCC "cc89" 89,
                          mCC "cc90" 90,
                          mCC "cc91" 91,
                          mCC "cc92" 92,
                          mCC "cc93" 93,
                          mCC "cc94" 94,
                          mCC "cc95" 95,
                          mCC "cc96" 96,
                          mCC "cc97" 97,
                          mCC "cc98" 98,
                          mCC "cc99" 99,
                          mCC "cc100" 100,
                          mCC "cc101" 101,
                          mCC "cc102" 102,
                          mCC "cc103" 103,
                          mCC "cc104" 104,
                          mCC "cc105" 105,
                          mCC "cc106" 106,
                          mCC "cc107" 107,
                          mCC "cc108" 108,
                          mCC "cc109" 109,
                          mCC "cc110" 110,
                          mCC "cc111" 111,
                          mCC "cc112" 112,
                          mCC "cc113" 113,
                          mCC "cc114" 114,
                          mCC "cc115" 115,
                          mCC "cc116" 116,
                          mCC "cc117" 117,
                          mCC "cc118" 118,
                          mCC "cc119" 119,
                          mCC "cc120" 120,
                          mCC "cc121" 121,
                          mCC "cc122" 122,
                          mCC "cc123" 123,
                          mCC "cc124" 124,
                          mCC "cc125" 125,
                          mCC "cc126" 126,
                          mCC "cc127" 127
                        ],
                        duration = ("dur", 0.05),
                        velocity = ("vel", 0.5),
                        latency = 0.1}

oscGenericMIDI = toOscShape genericmidi

note                = makeI oscGenericMIDI "note"
dur             	= makeF oscGenericMIDI "dur"
vel             	= makeF oscGenericMIDI "vel"
bankselect          = makeF oscGenericMIDI "bankselect"
modwheel            = makeF oscGenericMIDI "modwheel"
breath              = makeF oscGenericMIDI "breath"
foot                = makeF oscGenericMIDI "foot"
portamentotime      = makeF oscGenericMIDI "portamentotime"
volume              = makeF oscGenericMIDI "volume"
balance             = makeF oscGenericMIDI "balance"
pan                 = makeF oscGenericMIDI "pan"
expression          = makeF oscGenericMIDI "expression"
effectcontrol1      = makeF oscGenericMIDI "effectcontrol1"
effectcontrol2      = makeF oscGenericMIDI "effectcontrol2"
sustainpedal        = makeF oscGenericMIDI "sustainpedal"
portamento          = makeF oscGenericMIDI "portamento"
sostenuto           = makeF oscGenericMIDI "sostenuto"
softpedal           = makeF oscGenericMIDI "softpedal"
legato              = makeF oscGenericMIDI "legato"
hold2               = makeF oscGenericMIDI "hold2"
variation           = makeF oscGenericMIDI "variation"
vcf                 = makeF oscGenericMIDI "vcf"
vca                 = makeF oscGenericMIDI "vca"
attack              = makeF oscGenericMIDI "attack"
brightness          = makeF oscGenericMIDI "brightness"
portamentocontrol   = makeF oscGenericMIDI "portamentocontrol"
effects1depth       = makeF oscGenericMIDI "effects1depth"
effects2depth       = makeF oscGenericMIDI "effects2depth"
effects3depth       = makeF oscGenericMIDI "effects3depth"
effects4depth       = makeF oscGenericMIDI "effects4depth"
effects5depth       = makeF oscGenericMIDI "effects5depth"
reset               = makeF oscGenericMIDI "reset"
localcontrol        = makeF oscGenericMIDI "localcontrol"
allnotesoff         = makeF oscGenericMIDI "allnotesoff"
omnioff             = makeF oscGenericMIDI "omnioff"
omnion              = makeF oscGenericMIDI "omnion"
monoon              = makeF oscGenericMIDI "monoon"
polyon              = makeF oscGenericMIDI "polyon"

cc0               = makeF oscGenericMIDI "cc0"
cc1               = makeF oscGenericMIDI "cc1"
cc2               = makeF oscGenericMIDI "cc2"
cc3               = makeF oscGenericMIDI "cc3"
cc4               = makeF oscGenericMIDI "cc4"
cc5               = makeF oscGenericMIDI "cc5"
cc6               = makeF oscGenericMIDI "cc6"
cc7               = makeF oscGenericMIDI "cc7"
cc8               = makeF oscGenericMIDI "cc8"
cc9               = makeF oscGenericMIDI "cc9"
cc10              = makeF oscGenericMIDI "cc10"
cc11              = makeF oscGenericMIDI "cc11"
cc12              = makeF oscGenericMIDI "cc12"
cc13              = makeF oscGenericMIDI "cc13"
cc14              = makeF oscGenericMIDI "cc14"
cc15              = makeF oscGenericMIDI "cc15"
cc16              = makeF oscGenericMIDI "cc16"
cc17              = makeF oscGenericMIDI "cc17"
cc18              = makeF oscGenericMIDI "cc18"
cc19              = makeF oscGenericMIDI "cc19"
cc20              = makeF oscGenericMIDI "cc20"
cc21              = makeF oscGenericMIDI "cc21"
cc22              = makeF oscGenericMIDI "cc22"
cc23              = makeF oscGenericMIDI "cc23"
cc24              = makeF oscGenericMIDI "cc24"
cc25              = makeF oscGenericMIDI "cc25"
cc26              = makeF oscGenericMIDI "cc26"
cc27              = makeF oscGenericMIDI "cc27"
cc28              = makeF oscGenericMIDI "cc28"
cc29              = makeF oscGenericMIDI "cc29"
cc30              = makeF oscGenericMIDI "cc30"
cc31              = makeF oscGenericMIDI "cc31"
cc32              = makeF oscGenericMIDI "cc32"
cc33              = makeF oscGenericMIDI "cc33"
cc34              = makeF oscGenericMIDI "cc34"
cc35              = makeF oscGenericMIDI "cc35"
cc36              = makeF oscGenericMIDI "cc36"
cc37              = makeF oscGenericMIDI "cc37"
cc38              = makeF oscGenericMIDI "cc38"
cc39              = makeF oscGenericMIDI "cc39"
cc40              = makeF oscGenericMIDI "cc40"
cc41              = makeF oscGenericMIDI "cc41"
cc42              = makeF oscGenericMIDI "cc42"
cc43              = makeF oscGenericMIDI "cc43"
cc44              = makeF oscGenericMIDI "cc44"
cc45              = makeF oscGenericMIDI "cc45"
cc46              = makeF oscGenericMIDI "cc46"
cc47              = makeF oscGenericMIDI "cc47"
cc48              = makeF oscGenericMIDI "cc48"
cc49              = makeF oscGenericMIDI "cc49"
cc50              = makeF oscGenericMIDI "cc50"
cc51              = makeF oscGenericMIDI "cc51"
cc52              = makeF oscGenericMIDI "cc52"
cc53              = makeF oscGenericMIDI "cc53"
cc54              = makeF oscGenericMIDI "cc54"
cc55              = makeF oscGenericMIDI "cc55"
cc56              = makeF oscGenericMIDI "cc56"
cc57              = makeF oscGenericMIDI "cc57"
cc58              = makeF oscGenericMIDI "cc58"
cc59              = makeF oscGenericMIDI "cc59"
cc60              = makeF oscGenericMIDI "cc60"
cc61              = makeF oscGenericMIDI "cc61"
cc62              = makeF oscGenericMIDI "cc62"
cc63              = makeF oscGenericMIDI "cc63"
cc64              = makeF oscGenericMIDI "cc64"
cc65              = makeF oscGenericMIDI "cc65"
cc66              = makeF oscGenericMIDI "cc66"
cc67              = makeF oscGenericMIDI "cc67"
cc68              = makeF oscGenericMIDI "cc68"
cc69              = makeF oscGenericMIDI "cc69"
cc70              = makeF oscGenericMIDI "cc70"
cc71              = makeF oscGenericMIDI "cc71"
cc72              = makeF oscGenericMIDI "cc72"
cc73              = makeF oscGenericMIDI "cc73"
cc74              = makeF oscGenericMIDI "cc74"
cc75              = makeF oscGenericMIDI "cc75"
cc76              = makeF oscGenericMIDI "cc76"
cc77              = makeF oscGenericMIDI "cc77"
cc78              = makeF oscGenericMIDI "cc78"
cc79              = makeF oscGenericMIDI "cc79"
cc80              = makeF oscGenericMIDI "cc80"
cc81              = makeF oscGenericMIDI "cc81"
cc82              = makeF oscGenericMIDI "cc82"
cc83              = makeF oscGenericMIDI "cc83"
cc84              = makeF oscGenericMIDI "cc84"
cc85              = makeF oscGenericMIDI "cc85"
cc86              = makeF oscGenericMIDI "cc86"
cc87              = makeF oscGenericMIDI "cc87"
cc88              = makeF oscGenericMIDI "cc88"
cc89              = makeF oscGenericMIDI "cc89"
cc90              = makeF oscGenericMIDI "cc90"
cc91              = makeF oscGenericMIDI "cc91"
cc92              = makeF oscGenericMIDI "cc92"
cc93              = makeF oscGenericMIDI "cc93"
cc94              = makeF oscGenericMIDI "cc94"
cc95              = makeF oscGenericMIDI "cc95"
cc96              = makeF oscGenericMIDI "cc96"
cc97              = makeF oscGenericMIDI "cc97"
cc98              = makeF oscGenericMIDI "cc98"
cc99              = makeF oscGenericMIDI "cc99"
cc100             = makeF oscGenericMIDI "cc100"
cc101             = makeF oscGenericMIDI "cc101"
cc102             = makeF oscGenericMIDI "cc102"
cc103             = makeF oscGenericMIDI "cc103"
cc104             = makeF oscGenericMIDI "cc104"
cc105             = makeF oscGenericMIDI "cc105"
cc106             = makeF oscGenericMIDI "cc106"
cc107             = makeF oscGenericMIDI "cc107"
cc108             = makeF oscGenericMIDI "cc108"
cc109             = makeF oscGenericMIDI "cc109"
cc110             = makeF oscGenericMIDI "cc110"
cc111             = makeF oscGenericMIDI "cc111"
cc112             = makeF oscGenericMIDI "cc112"
cc113             = makeF oscGenericMIDI "cc113"
cc114             = makeF oscGenericMIDI "cc114"
cc115             = makeF oscGenericMIDI "cc115"
cc116             = makeF oscGenericMIDI "cc116"
cc117             = makeF oscGenericMIDI "cc117"
cc118             = makeF oscGenericMIDI "cc118"
cc119             = makeF oscGenericMIDI "cc119"
cc120             = makeF oscGenericMIDI "cc120"
cc121             = makeF oscGenericMIDI "cc121"
cc122             = makeF oscGenericMIDI "cc122"
cc123             = makeF oscGenericMIDI "cc123"
cc124             = makeF oscGenericMIDI "cc124"
cc125             = makeF oscGenericMIDI "cc125"
cc126             = makeF oscGenericMIDI "cc126"
cc127             = makeF oscGenericMIDI "cc127"
