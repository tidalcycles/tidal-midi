  
module Sound.Tidal.VolcaBass where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect
import GHC.Word
import GHC.Int

import Sound.OSC.FD
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.MVar
import Data.Hashable
import Data.Bits
import Data.Maybe
import System.Process
import Control.Concurrent

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse

channel = Event.Channel 0

bass :: OscShape
bass = OscShape {path = "/note",
                 params = [ I "note" Nothing,
                            F "dur" (Just (0.05)),
                            F "slide" (Just (-1)),
                            F "expression" (Just (-1)),
                            F "octave" (Just (-1)),
                            F "lforate" (Just (-1)),
                            F "lfoint" (Just (-1)),
                            F "pitch1" (Just (-1)),
                            F "pitch2" (Just (-1)),
                            F "pitch3" (Just (-1)),
                            F "attack" (Just (-1)),
                            F "decay" (Just (-1)),
                            F "cutoff" (Just (-1)),
                            F "gate" (Just (-1))
                          ],
                 timestamp = NoStamp,
                 latency = 0,
                 namedParams = False,
                 preamble = []
                }

bassStream = stream "127.0.0.1" 7303 bass

note         = makeI bass "note"
dur          = makeF bass "dur"
slide        = makeF bass "slide"
expression   = makeF bass "expression"
octave       = makeF bass "octave"
lforate      = makeF bass "lforate"
lfoint       = makeF bass "lfoint"
pitch1       = makeF bass "pitch1"
pitch2       = makeF bass "pitch2"
pitch3       = makeF bass "pitch3"
attack       = makeF bass "attack"
decay        = makeF bass "decay"
cutoff       = makeF bass "cutoff"
gate         = makeF bass "gate"

bassnames = map name (tail $ tail $ params bass)

bassproxy latency midiport = 
   do h <- SndSeq.openDefault SndSeq.Block
      Client.setName (h :: SndSeq.T SndSeq.OutputMode) "Tidal"
      c <- Client.getId h
      p <- Port.createSimple h "out"
           (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
      conn <- Connect.createTo h p =<< Addr.parse h midiport
      x <- udpServer "127.0.0.1" 7303
      forkIO $ loop h conn x
      return ()
         where loop h conn x = do m <- recvMessage x
                                  act h conn m
                                  loop h conn x
               act h conn (Just (Message "/note" (note:dur:ctrls))) = 
                   do -- print $ "Got note " ++ show note
                      let note' = (fromJust $ d_get note) :: Int
                          dur' = (fromJust $ d_get dur) :: Float
                          ctrls' = (map (fromJust . d_get) ctrls) :: [Float]
                      sendmidi latency h conn (fromIntegral note', dur') ctrls'
                      return ()

sendmidi latency h conn (note,dur) ctrls = 
  do forkIO $ do threadDelay latency
                 Event.outputDirect h $ noteOn conn note 60
                 threadDelay (floor $ 1000000 * dur)
                 Event.outputDirect h $ noteOff conn note
                 return ()
     let ctrls' = map (floor . (* 127)) ctrls
         ctrls'' = filter ((>=0) . snd) (zip bassnames ctrls')
     sequence_ $ map (\(name, ctrl) -> Event.outputDirect h $ makeCtrl conn (ctrlN name ctrl)) ctrls''
     return ()

ctrlN "slide" v         = (5, v)
ctrlN "expression" v    = (11, v)
ctrlN "octave" v        = (40, v)
ctrlN "lforate" v       = (41, v)
ctrlN "lfoint" v        = (42, v)
ctrlN "pitch1" v        = (43, v)
ctrlN "pitch2" v        = (44, v)
ctrlN "pitch3" v        = (45, v)
ctrlN "attack" v        = (46, v)
ctrlN "decay" v         = (47, v)
ctrlN "cutoff" v        = (48, v)
ctrlN "gate" v          = (49, v)
ctrlN s _               = error $ "no match for " ++ s

noteOn :: Connect.T -> Word8 -> Word8 -> Event.T
noteOn conn val vel = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOn
  $ Event.simpleNote channel
                     (Event.Pitch (val))
                     (Event.Velocity vel)

noteOff :: Connect.T -> Word8 -> Event.T
noteOff conn val = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOff
  $ Event.simpleNote channel
                     (Event.Pitch (val))
                     (Event.normalVelocity)

makeCtrl :: Connect.T -> (Word32, GHC.Int.Int32) -> Event.T
makeCtrl conn (c, n) = 
  Event.forConnection conn 
  $ Event.CtrlEv Event.Controller $ Event.Ctrl 
                                    channel 
                                    (Event.Parameter c) 
                                    (Event.Value n)

