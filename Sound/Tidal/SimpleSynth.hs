
module Sound.Tidal.SimpleSynth where

import qualified Sound.PortMidi as PM
import GHC.Word
import GHC.Int

import Sound.OSC.FD
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.MVar
--import Visual
import Data.Hashable
import Data.Bits
import Data.Maybe
import System.Process
import Control.Concurrent
import Foreign.C
import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse

import System.Environment
import System.IO.Error

import Control.Exception

-- channel = Event.Channel 0

keys :: OscShape
keys = OscShape {path = "/note",
                 params = [ I "note" Nothing,
                            F "dur" (Just (0.05)),
                            F "portamento" (Just (-1)),
                            F "expression" (Just (-1)),
                            F "voice" (Just (-1)),
                            F "octave" (Just (-1)),
                            F "detune" (Just (-1)),
                            F "vcoegint" (Just (-1)),
                            F "kcutoff" (Just (-1)),
                            F "vcfegint" (Just (-1)),
                            F "lforate" (Just (-1)),
                            F "lfopitchint" (Just (-1)),
                            F "lfocutoffint" (Just (-1)),
                            F "attack" (Just (-1)),
                            F "decay" (Just (-1)),
                            F "sustain" (Just (-1)),
                            F "dtime" (Just (-1)),
                            F "dfeedback" (Just (-1))
                          ],
                 timestamp = NoStamp,
                 latency = 0,
                 namedParams = False,
                 preamble = []
                }

keyStream = stream "127.0.0.1" 7303 keys

note         = makeI keys "note"
dur          = makeF keys "dur"
portamento   = makeF keys "portamento"
expression   = makeF keys "expression"
octave       = makeF keys "octave"
voice        = makeF keys "voice"
detune       = makeF keys "detune"
vcoegint     = makeF keys "vcoegint"
kcutoff      = makeF keys "kcutoff"
vcfegint     = makeF keys "vcfegint"
lforate      = makeF keys "lforate"
lfopitchint  = makeF keys "lfopitchint"
lfocutoffint = makeF keys "lfocutoffint"
attack       = makeF keys "attack"
decay        = makeF keys "decay"
sustain      = makeF keys "sustain"
dtime        = makeF keys "dtime"
dfeedback    = makeF keys "dfeedback"


keynames = map name (tail $ tail $ params keys)

-- msg = PMMsg 0x90 0x40 0x60
-- msgOff = PMMsg 0x92 0x40 0x00
-- allSoundOff = PMMsg 0xB0 0x78 0x00

withPortMidi :: IO a -> IO a
withPortMidi = bracket_ PM.initialize PM.terminate

test :: IO (Maybe a) -> IO a
test = (>>= maybe (ioError $ userError "oops") return)

getDefaultOutputFromEnv :: IO Int
getDefaultOutputFromEnv = do
  a <- catchIOError (getEnv "PM_RECOMMENDED_OUTPUT_DEVICE") missingHandler
  case a of
    [] -> do
      edev <- (test PM.getDefaultOutputDeviceID)
      putStrLn ("Using default Device with ID: " ++ show edev)
      return edev
    x -> return (read a)
    where
      missingHandler e = if isDoesNotExistError e
        then return ""
        else ioError e


keyproxy latency midiport = do
  PM.initialize
  do
    dev <- getDefaultOutputFromEnv
    result <- PM.openOutput dev 0
    case result of
      Right err -> putStrLn ("Failed opening Midi Output Port: " ++ show err)
      Left conn ->
        do
          x <- udpServer "127.0.0.1" 7303
          forkIO $ loop conn x
          return ()
            where loop conn x = do m <- recvMessage x
                                   act conn m
                                   loop conn x
                  act conn (Just (Message "/note" (note:dur:ctrls))) =
                      do
                        let note' = (fromJust $ d_get note) :: Int
                            dur' = (fromJust $ d_get dur) :: Float
                            ctrls' = (map (fromJust . d_get) ctrls) :: [Float]
                        -- putStrLn ("Message received")
                        sendmidi latency conn (fromIntegral note', dur') ctrls'
                        return()

sendmidi latency stream (note,dur) ctrls =
  do forkIO $ do threadDelay latency
                 noteOn stream note 60
                 --  putStrLn ("Send On Msg: " ++ show result)
                 threadDelay (floor $ 1000000 * dur)
                 noteOff stream note
                 --  putStrLn ("Send Off Msg: " ++ show result)
                 return ()
     let ctrls' = map (floor . (* 127)) ctrls
         ctrls'' = filter ((>=0) . snd) (zip keynames ctrls')
         --ctrls''' = map (\x -> (x, fromJust $ lookup x ctrls'')) usectrls
     --putStrLn $ show ctrls''
     sequence_ $ map (\(name, ctrl) -> makeCtrl stream (ctrlN name ctrl)) ctrls''
     return ()

ctrlN "portamento" v    = (5, v)
ctrlN "expression" v    = (11, v)
ctrlN "voice" v         = (31, v)
ctrlN "octave" v        = (27, v)
ctrlN "detune" v        = (29, v)
ctrlN "vcoegint" v      = (43, v)
ctrlN "kcutoff" v        = (69, v)
ctrlN "vcfegint" v      = (45, v)
ctrlN "lforate" v       = (16, v)
ctrlN "lfopitchint" v   = (47, v)
ctrlN "lfocutoffint" v  = (48, v)
ctrlN "attack" v        = (101, v)
ctrlN "decay" v         = (102, v)
ctrlN "sustain" v       = (103, v)
ctrlN "dtime" v         = (52, v)
ctrlN "dfeedback" v     = (53, v)
ctrlN s _               = error $ "no match for " ++ s


noteOn :: PM.PMStream -> CLong -> CLong -> IO PM.PMError
noteOn conn val vel = PM.writeShort conn evt
    where msg = PM.PMMsg 0x90 val vel
          evt = PM.PMEvent msg 0

noteOff :: PM.PMStream -> CLong -> IO PM.PMError
noteOff conn val = PM.writeShort conn evt
    where msg = PM.PMMsg 0x80 val 60
          evt = PM.PMEvent msg 0

makeCtrl :: PM.PMStream -> (CLong, CLong) -> IO PM.PMError
makeCtrl conn (c, n) = PM.writeShort conn evt
    where msg = PM.PMMsg 0xB0 c n
          evt = PM.PMEvent msg 0
