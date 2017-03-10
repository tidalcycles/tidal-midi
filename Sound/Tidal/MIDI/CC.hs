module Sound.Tidal.MIDI.CC where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

ccallController :: ControllerShape
ccallController =
  ControllerShape {controls = [
                            mCC cc0_p 0,
                            mCC cc1_p 1,
                            mCC cc2_p 2,
                            mCC cc3_p 3,
                            mCC cc4_p 4,
                            mCC cc5_p 5,
                            mCC cc6_p 6,
                            mCC cc7_p 7,
                            mCC cc8_p 8,
                            mCC cc9_p 9,
                            mCC cc10_p 10,
                            mCC cc11_p 11,
                            mCC cc12_p 12,
                            mCC cc13_p 13,
                            mCC cc14_p 14,
                            mCC cc15_p 15,
                            mCC cc16_p 16,
                            mCC cc17_p 17,
                            mCC cc18_p 18,
                            mCC cc19_p 19,
                            mCC cc20_p 20,
                            mCC cc21_p 21,
                            mCC cc22_p 22,
                            mCC cc23_p 23,
                            mCC cc24_p 24,
                            mCC cc25_p 25,
                            mCC cc26_p 26,
                            mCC cc27_p 27,
                            mCC cc28_p 28,
                            mCC cc29_p 29,
                            mCC cc30_p 30,
                            mCC cc31_p 31,
                            mCC cc32_p 32,
                            mCC cc33_p 33,
                            mCC cc34_p 34,
                            mCC cc35_p 35,
                            mCC cc36_p 36,
                            mCC cc37_p 37,
                            mCC cc38_p 38,
                            mCC cc39_p 39,
                            mCC cc40_p 40,
                            mCC cc41_p 41,
                            mCC cc42_p 42,
                            mCC cc43_p 43,
                            mCC cc44_p 44,
                            mCC cc45_p 45,
                            mCC cc46_p 46,
                            mCC cc47_p 47,
                            mCC cc48_p 48,
                            mCC cc49_p 49,
                            mCC cc50_p 50,
                            mCC cc51_p 51,
                            mCC cc52_p 52,
                            mCC cc53_p 53,
                            mCC cc54_p 54,
                            mCC cc55_p 55,
                            mCC cc56_p 56,
                            mCC cc57_p 57,
                            mCC cc58_p 58,
                            mCC cc59_p 59,
                            mCC cc60_p 60,
                            mCC cc61_p 61,
                            mCC cc62_p 62,
                            mCC cc63_p 63,
                            mCC cc64_p 64,
                            mCC cc65_p 65,
                            mCC cc66_p 66,
                            mCC cc67_p 67,
                            mCC cc68_p 68,
                            mCC cc69_p 69,
                            mCC cc70_p 70,
                            mCC cc71_p 71,
                            mCC cc72_p 72,
                            mCC cc73_p 73,
                            mCC cc74_p 74,
                            mCC cc75_p 75,
                            mCC cc76_p 76,
                            mCC cc77_p 77,
                            mCC cc78_p 78,
                            mCC cc79_p 79,
                            mCC cc80_p 80,
                            mCC cc81_p 81,
                            mCC cc82_p 82,
                            mCC cc83_p 83,
                            mCC cc84_p 84,
                            mCC cc85_p 85,
                            mCC cc86_p 86,
                            mCC cc87_p 87,
                            mCC cc89_p 89,
                            mCC cc90_p 90,
                            mCC cc91_p 91,
                            mCC cc92_p 92,
                            mCC cc93_p 93,
                            mCC cc94_p 94,
                            mCC cc95_p 95,
                            mCC cc96_p 96,
                            mCC cc97_p 97,
                            mCC cc98_p 98,
                            mCC cc99_p 99,
                            mCC cc100_p 100,
                            mCC cc101_p 101,
                            mCC cc102_p 102,
                            mCC cc103_p 103,
                            mCC cc104_p 104,
                            mCC cc105_p 105,
                            mCC cc106_p 106,
                            mCC cc107_p 107,
                            mCC cc108_p 108,
                            mCC cc109_p 109,
                            mCC cc110_p 110,
                            mCC cc111_p 111,
                            mCC cc112_p 112,
                            mCC cc113_p 113,
                            mCC cc114_p 114,
                            mCC cc115_p 115,
                            mCC cc116_p 116,
                            mCC cc117_p 117,
                            mCC cc118_p 118,
                            mCC cc119_p 119,
                            mCC cc120_p 120,
                            mCC cc121_p 121,
                            mCC cc122_p 122,
                            mCC cc123_p 123,
                            mCC cc124_p 124,
                            mCC cc125_p 125,
                            mCC cc126_p 126,
                            mCC cc127_p 127
                            ],
                       -- duration = ("dur", 0.05),
                       -- velocity = ("vel", 0.5),
                       latency = 0.1}

ccall = toShape ccallController

(cc0, cc0_p) = pF "cc0" (Just 0)
(cc1, cc1_p) = pF "cc1" (Just 0)
(cc2, cc2_p) = pF "cc2" (Just 0)
(cc3, cc3_p) = pF "cc3" (Just 0)
(cc4, cc4_p) = pF "cc4" (Just 0)
(cc5, cc5_p) = pF "cc5" (Just 0)
(cc6, cc6_p) = pF "cc6" (Just 0)
(cc7, cc7_p) = pF "cc7" (Just 0)
(cc8, cc8_p) = pF "cc8" (Just 0)
(cc9, cc9_p) = pF "cc9" (Just 0)
(cc10, cc10_p) = pF "cc10" (Just 0)
(cc11, cc11_p) = pF "cc11" (Just 0)
(cc12, cc12_p) = pF "cc12" (Just 0)
(cc13, cc13_p) = pF "cc13" (Just 0)
(cc14, cc14_p) = pF "cc14" (Just 0)
(cc15, cc15_p) = pF "cc15" (Just 0)
(cc16, cc16_p) = pF "cc16" (Just 0)
(cc17, cc17_p) = pF "cc17" (Just 0)
(cc18, cc18_p) = pF "cc18" (Just 0)
(cc19, cc19_p) = pF "cc19" (Just 0)
(cc20, cc20_p) = pF "cc20" (Just 0)
(cc21, cc21_p) = pF "cc21" (Just 0)
(cc22, cc22_p) = pF "cc22" (Just 0)
(cc23, cc23_p) = pF "cc23" (Just 0)
(cc24, cc24_p) = pF "cc24" (Just 0)
(cc25, cc25_p) = pF "cc25" (Just 0)
(cc26, cc26_p) = pF "cc26" (Just 0)
(cc27, cc27_p) = pF "cc27" (Just 0)
(cc28, cc28_p) = pF "cc28" (Just 0)
(cc29, cc29_p) = pF "cc29" (Just 0)
(cc30, cc30_p) = pF "cc30" (Just 0)
(cc31, cc31_p) = pF "cc31" (Just 0)
(cc32, cc32_p) = pF "cc32" (Just 0)
(cc33, cc33_p) = pF "cc33" (Just 0)
(cc34, cc34_p) = pF "cc34" (Just 0)
(cc35, cc35_p) = pF "cc35" (Just 0)
(cc36, cc36_p) = pF "cc36" (Just 0)
(cc37, cc37_p) = pF "cc37" (Just 0)
(cc38, cc38_p) = pF "cc38" (Just 0)
(cc39, cc39_p) = pF "cc39" (Just 0)
(cc40, cc40_p) = pF "cc40" (Just 0)
(cc41, cc41_p) = pF "cc41" (Just 0)
(cc42, cc42_p) = pF "cc42" (Just 0)
(cc43, cc43_p) = pF "cc43" (Just 0)
(cc44, cc44_p) = pF "cc44" (Just 0)
(cc45, cc45_p) = pF "cc45" (Just 0)
(cc46, cc46_p) = pF "cc46" (Just 0)
(cc47, cc47_p) = pF "cc47" (Just 0)
(cc48, cc48_p) = pF "cc48" (Just 0)
(cc49, cc49_p) = pF "cc49" (Just 0)
(cc50, cc50_p) = pF "cc50" (Just 0)
(cc51, cc51_p) = pF "cc51" (Just 0)
(cc52, cc52_p) = pF "cc52" (Just 0)
(cc53, cc53_p) = pF "cc53" (Just 0)
(cc54, cc54_p) = pF "cc54" (Just 0)
(cc55, cc55_p) = pF "cc55" (Just 0)
(cc56, cc56_p) = pF "cc56" (Just 0)
(cc57, cc57_p) = pF "cc57" (Just 0)
(cc58, cc58_p) = pF "cc58" (Just 0)
(cc59, cc59_p) = pF "cc59" (Just 0)
(cc60, cc60_p) = pF "cc60" (Just 0)
(cc61, cc61_p) = pF "cc61" (Just 0)
(cc62, cc62_p) = pF "cc62" (Just 0)
(cc63, cc63_p) = pF "cc63" (Just 0)
(cc64, cc64_p) = pF "cc64" (Just 0)
(cc65, cc65_p) = pF "cc65" (Just 0)
(cc66, cc66_p) = pF "cc66" (Just 0)
(cc67, cc67_p) = pF "cc67" (Just 0)
(cc68, cc68_p) = pF "cc68" (Just 0)
(cc69, cc69_p) = pF "cc69" (Just 0)
(cc70, cc70_p) = pF "cc70" (Just 0)
(cc71, cc71_p) = pF "cc71" (Just 0)
(cc72, cc72_p) = pF "cc72" (Just 0)
(cc73, cc73_p) = pF "cc73" (Just 0)
(cc74, cc74_p) = pF "cc74" (Just 0)
(cc75, cc75_p) = pF "cc75" (Just 0)
(cc76, cc76_p) = pF "cc76" (Just 0)
(cc77, cc77_p) = pF "cc77" (Just 0)
(cc78, cc78_p) = pF "cc78" (Just 0)
(cc79, cc79_p) = pF "cc79" (Just 0)
(cc80, cc80_p) = pF "cc80" (Just 0)
(cc81, cc81_p) = pF "cc81" (Just 0)
(cc82, cc82_p) = pF "cc82" (Just 0)
(cc83, cc83_p) = pF "cc83" (Just 0)
(cc84, cc84_p) = pF "cc84" (Just 0)
(cc85, cc85_p) = pF "cc85" (Just 0)
(cc86, cc86_p) = pF "cc86" (Just 0)
(cc87, cc87_p) = pF "cc87" (Just 0)
(cc88, cc88_p) = pF "cc88" (Just 0)
(cc89, cc89_p) = pF "cc89" (Just 0)
(cc90, cc90_p) = pF "cc90" (Just 0)
(cc91, cc91_p) = pF "cc91" (Just 0)
(cc92, cc92_p) = pF "cc92" (Just 0)
(cc93, cc93_p) = pF "cc93" (Just 0)
(cc94, cc94_p) = pF "cc94" (Just 0)
(cc95, cc95_p) = pF "cc95" (Just 0)
(cc96, cc96_p) = pF "cc96" (Just 0)
(cc97, cc97_p) = pF "cc97" (Just 0)
(cc98, cc98_p) = pF "cc98" (Just 0)
(cc99, cc99_p) = pF "cc99" (Just 0)
(cc100, cc100_p) = pF "cc100" (Just 0)
(cc101, cc101_p) = pF "cc101" (Just 0)
(cc102, cc102_p) = pF "cc102" (Just 0)
(cc103, cc103_p) = pF "cc103" (Just 0)
(cc104, cc104_p) = pF "cc104" (Just 0)
(cc105, cc105_p) = pF "cc105" (Just 0)
(cc106, cc106_p) = pF "cc106" (Just 0)
(cc107, cc107_p) = pF "cc107" (Just 0)
(cc108, cc108_p) = pF "cc108" (Just 0)
(cc109, cc109_p) = pF "cc109" (Just 0)
(cc110, cc110_p) = pF "cc110" (Just 0)
(cc111, cc111_p) = pF "cc111" (Just 0)
(cc112, cc112_p) = pF "cc112" (Just 0)
(cc113, cc113_p) = pF "cc113" (Just 0)
(cc114, cc114_p) = pF "cc114" (Just 0)
(cc115, cc115_p) = pF "cc115" (Just 0)
(cc116, cc116_p) = pF "cc116" (Just 0)
(cc117, cc117_p) = pF "cc117" (Just 0)
(cc118, cc118_p) = pF "cc118" (Just 0)
(cc119, cc119_p) = pF "cc119" (Just 0)
(cc120, cc120_p) = pF "cc120" (Just 0)
(cc121, cc121_p) = pF "cc121" (Just 0)
(cc122, cc122_p) = pF "cc122" (Just 0)
(cc123, cc123_p) = pF "cc123" (Just 0)
(cc124, cc124_p) = pF "cc124" (Just 0)
(cc125, cc125_p) = pF "cc125" (Just 0)
(cc126, cc126_p) = pF "cc126" (Just 0)
(cc127, cc127_p) = pF "cc127" (Just 0)
