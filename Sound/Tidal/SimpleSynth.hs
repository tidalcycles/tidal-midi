module Sound.Tidal.SimpleSynth where

import qualified Sound.PortMidi as PM

import Sound.OSC.FD

import Control.Monad
import Control.Concurrent.MVar

import Data.Bits
import Data.Maybe

import Control.Concurrent
import Foreign.C

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse

import System.IO.Error


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

data Output = Output {
                       conn :: PM.PMStream,
                       lock :: MVar ()
                     }

outputDevice :: PM.DeviceID -> IO (Either Output PM.PMError)
outputDevice deviceID = do
  PM.initialize
  result <- PM.openOutput deviceID 0
  case result of
    Left dev ->
      do
        sem <- newEmptyMVar
        putMVar sem () -- initially fill MVar to be taken by the first user of this output
        return (Left Output { conn=dev, lock=sem })
    Right err -> return (Right err)

messageLoop oStream oLatency oChannel iPort = do
  putStrLn ("Starting message loop on port " ++ show iPort ++ " for MIDI channel " ++ show oChannel)
  x <- udpServer "127.0.0.1" iPort
  forkIO $ loop oStream x oChannel
    where loop oStream x oChannel = do m <- recvMessage x
                                       act oStream m oChannel
                                       loop oStream x oChannel
          act oStream (Just (Message "/note" (note:dur:ctrls))) oChannel =
              do
                let note' = (fromJust $ d_get note) :: Int
                    dur' = (fromJust $ d_get dur) :: Float
                    ctrls' = (map (fromJust . d_get) ctrls) :: [Float]
                -- putStrLn ("Message received")
                sendmidi oLatency oStream oChannel (fromIntegral note', dur') ctrls'
                return()


makeStream keys port = stream "127.0.0.1" port keys

keyproxy latency deviceID channels = do
  let ports = (map (+ 7303) channels)
      keyStreams = map (makeStream keys) ports
  econn <- outputDevice deviceID

  case econn of
    Right err -> error ("Failed opening MIDI Output on Device ID: " ++ show deviceID ++ " - " ++ show err)
    Left conn ->
      do
        zipWithM_ (messageLoop conn latency) channels ports
        return keyStreams


sendmidi latency stream channel (note,dur) ctrls =
  do forkIO $ do threadDelay latency
                 noteOn stream channel note 60
                 --  putStrLn ("Send On Msg: " ++ show result)
                 threadDelay (floor $ 1000000 * dur)
                 noteOff stream channel note
                 --  putStrLn ("Send Off Msg: " ++ show result)
                 return ()
     let ctrls' = map (floor . (* 127)) ctrls
         ctrls'' = filter ((>=0) . snd) (zip keynames ctrls')
         --ctrls''' = map (\x -> (x, fromJust $ lookup x ctrls'')) usectrls
     --putStrLn $ show ctrls''
     sequence_ $ map (\(name, ctrl) -> makeCtrl stream channel (ctrlN name ctrl)) ctrls''
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

-- PortMIDI Wrapper
makeEvent :: (Integral a, Integral b, Bits b) => b -> a -> b -> a -> a -> PM.PMEvent
makeEvent st n ch v t = PM.PMEvent msg (fromIntegral t)
  where msg = PM.PMMsg (fromIntegral $ encodeChannel ch st) (fromIntegral n) (fromIntegral v)

-- now with a semaphore since PortMIDI is NOT thread safe
sendEvent :: Output -> PM.PMEvent -> IO (Maybe IOError)
sendEvent o evt = do
  let sem = lock o
  takeMVar sem
  err <- PM.writeShort (conn o) evt
  putMVar sem ()
  case err of
    PM.NoError -> return Nothing
    e -> return $ Just (userError ("Error '" ++ show e ++ "' sending Event: " ++ show evt))

-- MIDI Utils
encodeChannel :: (Bits a, Integral a, Integral b) => a -> a -> b
encodeChannel ch cc = fromIntegral (((-) ch 1) .|. cc)

-- MIDI Messages
noteOn :: (Bits a, Integral a, Integral b) => Output -> a -> b -> b -> IO (Maybe IOError)
noteOn o ch val vel = do
  let evt = makeEvent 0x90 val ch vel 0
  sendEvent o evt

noteOff :: (Bits a, Integral a, Integral b) => Output -> a -> b -> IO (Maybe IOError)
noteOff o ch val = do
  let evt = makeEvent 0x80 val ch 60 0
  sendEvent o evt

-- This is sending NRPN
makeCtrl :: (Bits a, Integral a) => Output -> a -> (CLong, CLong) -> IO (Maybe IOError)
makeCtrl o ch (c, n) = do
  let evt = makeEvent 0xB0 c ch n 0
  sendEvent o evt
  --
  -- let nrpn = makeEvent 0xB0
  --     evts = [nrpn 0x63 ch (shift (c .&. 0x3F80) (-7)) 0,
  --             nrpn 0x62 ch (c .&. 0x7F)  0,
  --             nrpn 0x06 ch (shift (n .&. 0x3F80) (-7)) 0,
  --             nrpn 0x26 ch (n .&. 0x7F) 0
  --            ]
  -- maybeErrors <- mapM (sendEvent o) evts
  -- let errors = catMaybes maybeErrors
  -- case (length errors) of
  --   0 -> return Nothing
  --   x -> return $ Just $ userError (show x ++ " errors occurred while sending NRPN Event" ++ show evts)
