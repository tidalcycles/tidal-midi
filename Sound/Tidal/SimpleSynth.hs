module Sound.Tidal.SimpleSynth where

import qualified Sound.PortMidi as PM

import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX

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
import Sound.Tidal.Tempo

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
                 timestamp = MessageStamp,
                 latency = 0.1,
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
                       lock :: MVar (),
                       offset :: (Integer, Integer)
                     }

outputDevice :: PM.DeviceID -> Int -> IO (Either Output PM.PMError)
outputDevice deviceID latency = do
  PM.initialize
  result <- PM.openOutput deviceID 1
  case result of
    Left dev ->
      do
        sem <- newEmptyMVar
        putMVar sem () -- initially fill MVar to be taken by the first user of this output
        midiOffset <- PM.time -- in milliseconds

        now <- getCurrentTime
        let posixNow = realToFrac $ utcTimeToPOSIXSeconds now
            syncedNow = posixNow - ((0.001*) $ fromIntegral midiOffset)
            sec = floor syncedNow
            usec = floor $ 1000000 * (syncedNow - (realToFrac sec))
        return (Left Output { conn=dev, lock=sem, offset=(sec, usec) })
    Right err -> return (Right err)

timeDiff :: (Datum, Datum) -> (Integer, Integer) -> Integer
timeDiff (ds, du) (s, u) = diff d i
    where diff a b = floor ((a - b) * 1000) -- as millis
          d = asFrac jds jdu
          i = asFrac s u
          jds = (fromJust $ d_get ds) :: Integer
          jdu = (fromJust $ d_get du) :: Integer
          asFrac a b = (fromIntegral a) + ((fromIntegral b) * 0.000001)


messageLoop oStream oChannel iPort = do
  putStrLn ("Starting message loop on port " ++ show iPort ++ " for MIDI channel " ++ show oChannel)
  x <- udpServer "127.0.0.1" iPort
  forkIO $ loop oStream x oChannel
    where loop oStream x oChannel = do m <- recvMessage x
                                       act oStream m oChannel
                                       loop oStream x oChannel
          act oStream (Just (Message "/note" (sec:usec:note:dur:ctrls))) oChannel =
              do
                let diff = timeDiff (sec, usec) (offset oStream)
                    note' = (fromJust $ d_get note) :: Int
                    dur' = (fromJust $ d_get dur) :: Float
                    ctrls' = (map (fromJust . d_get) ctrls) :: [Float]

                -- currentTime <- PM.time
                -- putStrLn ("Will play MIDI note in " ++ show (diff - fromIntegral currentTime) ++ "ms")
                sendmidi oStream oChannel (fromIntegral note', dur') (diff) ctrls'
                return()


makeStream keys port = stream "127.0.0.1" port keys

keyproxy latency deviceID channels = do
  let ports = (map (+ 7303) channels)
      keyStreams = map (makeStream keys) ports
  econn <- outputDevice deviceID latency

  case econn of
    Right err -> error ("Failed opening MIDI Output on Device ID: " ++ show deviceID ++ " - " ++ show err)
    Left conn ->
      do
        zipWithM_ (messageLoop conn) channels ports
        currentTime <- getCurrentTime

        putStrLn ("Output Device: " ++ show (offset conn) ++ " - " ++ show (utcTimeToPOSIXSeconds currentTime))
        return keyStreams


sendmidi stream channel (note,dur) t ctrls =
  do forkIO $ do noteOn stream channel note 60 t
                 noteOff stream channel note (t + (floor $ 1000 * dur))
                 return ()
     let ctrls' = map (floor . (* 127)) ctrls
         ctrls'' = filter ((>=0) . snd) (zip keynames ctrls')
     sequence_ $ map (\(name, ctrl) -> makeCtrl stream channel (ctrlN name ctrl) t) ctrls''
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
noteOn :: (Bits a, Integral a, Integral b) => Output -> a -> b -> b -> b -> IO (Maybe IOError)
noteOn o ch val vel t = do
  let evt = makeEvent 0x90 val ch vel t
  sendEvent o evt

noteOff :: (Bits a, Integral a, Integral b) => Output -> a -> b -> b -> IO (Maybe IOError)
noteOff o ch val t = do
  let evt = makeEvent 0x80 val ch 60 t
  sendEvent o evt

-- This is sending NRPN
makeCtrl :: (Bits a, Integral a, Integral b) => Output -> a -> (b, b) -> b -> IO (Maybe IOError)
makeCtrl o ch (c, n) t = do
  let evt = makeEvent 0xB0 c ch n t
  sendEvent o evt
