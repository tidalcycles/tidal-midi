--This module is based on the SimpleSynth module
--https://github.com/tidalcycles/tidal-midi/blob/master/doc/synth-mapping.md
--After build problems, trying to base on VolcaKeys module 

module Sound.Tidal.MIDI.JamJarDrums where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

jjdController :: ControllerShape
jjdController = ControllerShape { controls = [
							mCC bcbd_p 32,
							mCC	bcsn_p 33,
							mCC bchc_p 34,
							mCC bcho_p 35,
							mCC bcfx1_p 36,
							mCC bcfx2_p 37,
							mCC bcfx3_p 38,
							
							
							mCC fqbd_p 48,
							mCC	fqsn_p 49,
							mCC fqhc_p 50,
							mCC fqho_p 51,
							mCC fqfx1_p 52,
							mCC fqfx2_p 53,
							mCC fqfx3_p 54,
							
							
							mCC stbd_p 65,
							mCC	stsn_p 66,
							mCC sthc_p 67,
							mCC stho_p 68,
							mCC stfx1_p 69,
							mCC stfx2_p 70,
							mCC stfx3_p 71,
							
							
							mCC lpbd_p 81,
							mCC	lpsn_p 82,
							mCC lphc_p 83,
							mCC lpho_p 84,
							mCC lpfx1_p 85,
							mCC lpfx2_p 86,
							mCC lpfx3_p 87
							
                          ],
                         latency = 0.01
                       }
                       
jjd = toShape jjdController

(bcbd,	bcbd_p)		= pF "bcbd" 	(Just 0)
(bcsn,	bcsn_p)		= pF "bcsn"		(Just 0)
(bchc,	bchc_p)		= pF "bchc"		(Just 0)
(bcho,	bcho_p)		= pF "bcho"		(Just 0)
(bcfx1,	bcfx1_p)	= pF "bcfx1"	(Just 0)
(bcfx2,	bcfx2_p)	= pF "bcfx2"	(Just 0)
(bcfx3,	bcfx3_p)	= pF "bcfx3"	(Just 0)


(fqbd,	fqbd_p)		= pF "fqbd" 	(Just 0)
(fqsn,	fqsn_p)		= pF "fqsn"		(Just 0)
(fqhc,	fqhc_p)		= pF "fqhc"		(Just 0)
(fqho,	fqho_p)		= pF "fqho"		(Just 0)
(fqfx1,	fqfx1_p)	= pF "fqfx1"	(Just 0)
(fqfx2,	fqfx2_p)	= pF "fqfx2"	(Just 0)
(fqfx3,	fqfx3_p)	= pF "fqfx3"	(Just 0)


(stbd,	stbd_p)		= pF "stbd" 	(Just 0)
(stsn,	stsn_p)		= pF "stsn"		(Just 0)
(sthc,	sthc_p)		= pF "sthc"		(Just 0)
(stho,	stho_p)		= pF "stho"		(Just 0)
(stfx1,	stfx1_p)	= pF "stfx1"	(Just 0)
(stfx2,	stfx2_p)	= pF "stfx2"	(Just 0)
(stfx3,	stfx3_p)	= pF "stfx3"	(Just 0)


(lpbd,	lpbd_p)		= pF "lpbd" 	(Just 0)
(lpsn,	lpsn_p)		= pF "lpsn"		(Just 0)
(lphc,	lphc_p)		= pF "lphc"		(Just 0)
(lpho,	lpho_p)		= pF "lpho"		(Just 0)
(lpfx1,	lpfx1_p)	= pF "lpfx1"	(Just 0)
(lpfx2,	lpfx2_p)	= pF "lpfx2"	(Just 0)
(lpfx3,	lpfx3_p)	= pF "lpfx3"	(Just 0)


