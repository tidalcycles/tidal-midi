module Sound.Tidal.GenericMIDI where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

anode :: ControllerShape
anode = ControllerShape {params = [

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
                          mCC "0" 0,
                          mCC "1" 1,
                          mCC "2" 2,
                          mCC "3" 3,
                          mCC "4" 4,
                          mCC "5" 5,
                          mCC "6" 6,
                          mCC "7" 7,
                          mCC "8" 8,
                          mCC "9" 9,
                          mCC "10" 10,
                          mCC "11" 11,
                          mCC "12" 12,
                          mCC "13" 13,
                          mCC "14" 14,
                          mCC "15" 15,
                          mCC "16" 16,
                          mCC "17" 17,
                          mCC "18" 18,
                          mCC "19" 19,
                          mCC "20" 20,
                          mCC "21" 21,
                          mCC "22" 22,
                          mCC "23" 23,
                          mCC "24" 24,
                          mCC "25" 25,
                          mCC "26" 26,
                          mCC "27" 27,
                          mCC "28" 28,
                          mCC "29" 29,
                          mCC "30" 30,
                          mCC "31" 31,
                          mCC "32" 32,
                          mCC "33" 33,
                          mCC "34" 34,
                          mCC "35" 35,
                          mCC "36" 36,
                          mCC "37" 37,
                          mCC "38" 38,
                          mCC "39" 39,
                          mCC "40" 40,
                          mCC "41" 41,
                          mCC "42" 42,
                          mCC "43" 43,
                          mCC "44" 44,
                          mCC "45" 45,
                          mCC "46" 46,
                          mCC "47" 47,
                          mCC "48" 48,
                          mCC "49" 49,
                          mCC "50" 50,
                          mCC "51" 51,
                          mCC "52" 52,
                          mCC "53" 53,
                          mCC "54" 54,
                          mCC "55" 55,
                          mCC "56" 56,
                          mCC "57" 57,
                          mCC "58" 58,
                          mCC "59" 59,
                          mCC "60" 60,
                          mCC "61" 61,
                          mCC "62" 62,
                          mCC "63" 63,
                          mCC "64" 64,
                          mCC "65" 65,
                          mCC "66" 66,
                          mCC "67" 67,
                          mCC "68" 68,
                          mCC "69" 69,
                          mCC "70" 70,
                          mCC "71" 71,
                          mCC "72" 72,
                          mCC "73" 73,
                          mCC "74" 74,
                          mCC "75" 75,
                          mCC "76" 76,
                          mCC "77" 77,
                          mCC "78" 78,
                          mCC "79" 79,
                          mCC "80" 80,
                          mCC "81" 81,
                          mCC "82" 82,
                          mCC "83" 83,
                          mCC "84" 84,
                          mCC "85" 85,
                          mCC "86" 86,
                          mCC "87" 87,
                          mCC "88" 88,
                          mCC "89" 89,
                          mCC "90" 90,
                          mCC "91" 91,
                          mCC "92" 92,
                          mCC "93" 93,
                          mCC "94" 94,
                          mCC "95" 95,
                          mCC "96" 96,
                          mCC "97" 97,
                          mCC "98" 98,
                          mCC "99" 99,
                          mCC "100" 100,
                          mCC "101" 101,
                          mCC "102" 102,
                          mCC "103" 103,
                          mCC "104" 104,
                          mCC "105" 105,
                          mCC "106" 106,
                          mCC "107" 107,
                          mCC "108" 108,
                          mCC "109" 109,
                          mCC "110" 110,
                          mCC "111" 111,
                          mCC "112" 112,
                          mCC "113" 113,
                          mCC "114" 114,
                          mCC "115" 115,
                          mCC "116" 116,
                          mCC "117" 117,
                          mCC "118" 118,
                          mCC "119" 119,
                          mCC "120" 120,
                          mCC "121" 121,
                          mCC "122" 122,
                          mCC "123" 123,
                          mCC "124" 124,
                          mCC "125" 125,
                          mCC "126" 126,
                          mCC "127" 127
                        ],
                        duration = ("dur", 0.05),
                        velocity = ("vel", 0.5),
                        latency = 0.1}

oscGenericMIDI = toOscShape genericmidi

note                = makeI oscGenericMIDI "note"

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

0               = makeF oscGenericMIDI "0"
1               = makeF oscGenericMIDI "1"
2               = makeF oscGenericMIDI "2"
3               = makeF oscGenericMIDI "3"
4               = makeF oscGenericMIDI "4"
5               = makeF oscGenericMIDI "5"
6               = makeF oscGenericMIDI "6"
7               = makeF oscGenericMIDI "7"
8               = makeF oscGenericMIDI "8"
9               = makeF oscGenericMIDI "9"
10              = makeF oscGenericMIDI "10"
11              = makeF oscGenericMIDI "11"
12              = makeF oscGenericMIDI "12"
13              = makeF oscGenericMIDI "13"
14              = makeF oscGenericMIDI "14"
15              = makeF oscGenericMIDI "15"
16              = makeF oscGenericMIDI "16"
17              = makeF oscGenericMIDI "17"
18              = makeF oscGenericMIDI "18"
19              = makeF oscGenericMIDI "19"
20              = makeF oscGenericMIDI "20"
21              = makeF oscGenericMIDI "21"
22              = makeF oscGenericMIDI "22"
23              = makeF oscGenericMIDI "23"
24              = makeF oscGenericMIDI "24"
25              = makeF oscGenericMIDI "25"
26              = makeF oscGenericMIDI "26"
27              = makeF oscGenericMIDI "27"
28              = makeF oscGenericMIDI "28"
29              = makeF oscGenericMIDI "29"
30              = makeF oscGenericMIDI "30"
31              = makeF oscGenericMIDI "31"
32              = makeF oscGenericMIDI "32"
33              = makeF oscGenericMIDI "33"
34              = makeF oscGenericMIDI "34"
35              = makeF oscGenericMIDI "35"
36              = makeF oscGenericMIDI "36"
37              = makeF oscGenericMIDI "37"
38              = makeF oscGenericMIDI "38"
39              = makeF oscGenericMIDI "39"
40              = makeF oscGenericMIDI "40"
41              = makeF oscGenericMIDI "41"
42              = makeF oscGenericMIDI "42"
43              = makeF oscGenericMIDI "43"
44              = makeF oscGenericMIDI "44"
45              = makeF oscGenericMIDI "45"
46              = makeF oscGenericMIDI "46"
47              = makeF oscGenericMIDI "47"
48              = makeF oscGenericMIDI "48"
49              = makeF oscGenericMIDI "49"
50              = makeF oscGenericMIDI "50"
51              = makeF oscGenericMIDI "51"
52              = makeF oscGenericMIDI "52"
53              = makeF oscGenericMIDI "53"
54              = makeF oscGenericMIDI "54"
55              = makeF oscGenericMIDI "55"
56              = makeF oscGenericMIDI "56"
57              = makeF oscGenericMIDI "57"
58              = makeF oscGenericMIDI "58"
59              = makeF oscGenericMIDI "59"
60              = makeF oscGenericMIDI "60"
61              = makeF oscGenericMIDI "61"
62              = makeF oscGenericMIDI "62"
63              = makeF oscGenericMIDI "63"
64              = makeF oscGenericMIDI "64"
65              = makeF oscGenericMIDI "65"
66              = makeF oscGenericMIDI "66"
67              = makeF oscGenericMIDI "67"
68              = makeF oscGenericMIDI "68"
69              = makeF oscGenericMIDI "69"
70              = makeF oscGenericMIDI "70"
71              = makeF oscGenericMIDI "71"
72              = makeF oscGenericMIDI "72"
73              = makeF oscGenericMIDI "73"
74              = makeF oscGenericMIDI "74"
75              = makeF oscGenericMIDI "75"
76              = makeF oscGenericMIDI "76"
77              = makeF oscGenericMIDI "77"
78              = makeF oscGenericMIDI "78"
79              = makeF oscGenericMIDI "79"
80              = makeF oscGenericMIDI "80"
81              = makeF oscGenericMIDI "81"
82              = makeF oscGenericMIDI "82"
83              = makeF oscGenericMIDI "83"
84              = makeF oscGenericMIDI "84"
85              = makeF oscGenericMIDI "85"
86              = makeF oscGenericMIDI "86"
87              = makeF oscGenericMIDI "87"
88              = makeF oscGenericMIDI "88"
89              = makeF oscGenericMIDI "89"
90              = makeF oscGenericMIDI "90"
91              = makeF oscGenericMIDI "91"
92              = makeF oscGenericMIDI "92"
93              = makeF oscGenericMIDI "93"
94              = makeF oscGenericMIDI "94"
95              = makeF oscGenericMIDI "95"
96              = makeF oscGenericMIDI "96"
97              = makeF oscGenericMIDI "97"
98              = makeF oscGenericMIDI "98"
99              = makeF oscGenericMIDI "99"
100             = makeF oscGenericMIDI "100"
101             = makeF oscGenericMIDI "101"
102             = makeF oscGenericMIDI "102"
103             = makeF oscGenericMIDI "103"
104             = makeF oscGenericMIDI "104"
105             = makeF oscGenericMIDI "105"
106             = makeF oscGenericMIDI "106"
107             = makeF oscGenericMIDI "107"
108             = makeF oscGenericMIDI "108"
109             = makeF oscGenericMIDI "109"
110             = makeF oscGenericMIDI "110"
111             = makeF oscGenericMIDI "111"
112             = makeF oscGenericMIDI "112"
113             = makeF oscGenericMIDI "113"
114             = makeF oscGenericMIDI "114"
115             = makeF oscGenericMIDI "115"
116             = makeF oscGenericMIDI "116"
117             = makeF oscGenericMIDI "117"
118             = makeF oscGenericMIDI "118"
119             = makeF oscGenericMIDI "119"
120             = makeF oscGenericMIDI "120"
121             = makeF oscGenericMIDI "121"
122             = makeF oscGenericMIDI "122"
123             = makeF oscGenericMIDI "123"
124             = makeF oscGenericMIDI "124"
125             = makeF oscGenericMIDI "125"
126             = makeF oscGenericMIDI "126"
127             = makeF oscGenericMIDI "127"
