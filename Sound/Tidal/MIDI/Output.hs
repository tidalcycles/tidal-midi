module Sound.Tidal.MIDI.Output where

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

import qualified Sound.Tidal.MIDI.Control as C

import Sound.Tidal.Pattern
import Sound.Tidal.Parse
import Sound.Tidal.Tempo
import qualified Sound.Tidal.Stream as S (stream, name, params)

import System.IO.Error

data Output = Output {
                       conn :: PM.PMStream,
                       lock :: MVar (),
                       offset :: (Integer, Integer)
                     }


messageLoop stream shape ch port = do
  putStrLn ("Starting message loop on port " ++ show port ++ " for MIDI channel " ++ show ch)
  x <- udpServer "127.0.0.1" port
  forkIO $ loop stream x ch
    where loop stream x ch = do m <- recvMessage x
                                act stream m ch
                                loop stream x ch
          act stream (Just (Message "/note" (sec:usec:note:dur:ctrls))) ch =
              do
                let diff = timeDiff (sec, usec) (offset stream)
                    note' = (fromJust $ d_get note) :: Int
                    dur' = (fromJust $ d_get dur) :: Float
                    ctrls' = (map (fromJust . d_get) ctrls) :: [Float]

                -- currentTime <- PM.time
                -- putStrLn ("Will play MIDI note in " ++ show (diff - fromIntegral currentTime) ++ "ms")
                sendmidi stream shape ch (fromIntegral note', dur') (diff) ctrls'
                return()


makeStream shape port = S.stream "127.0.0.1" port shape

-- ctrlN s _               = error $ "no match for " ++ s

keyproxy latency deviceID shape channels = do
  let ports = (map (+ 7303) channels)
      keyStreams = map (makeStream (C.toOscShape shape)) ports
  econn <- outputDevice deviceID latency

  case econn of
    Right err -> error ("Failed opening MIDI Output on Device ID: " ++ show deviceID ++ " - " ++ show err)
    Left conn ->
      do
        zipWithM_ (messageLoop conn shape) channels ports
        currentTime <- getCurrentTime

        putStrLn ("Output Device: " ++ show (offset conn) ++ " - " ++ show (utcTimeToPOSIXSeconds currentTime))
        return keyStreams


sendmidi stream shape ch (note,dur) t ctrls =
  do forkIO $ do noteOn stream ch note 60 t
                 noteOff stream ch note (t + (floor $ 1000 * dur))
                 return ()
     let ctrls' = filter ((>=0) . snd) (zip (C.toKeynames shape) ctrls)
     sequence_ $ map (\(name, ctrl) -> makeCtrl stream ch (C.paramN shape name) ctrl t) ctrls'
     return ()

timeDiff :: Integral a => (Datum, Datum) -> (Integer, Integer) -> a
timeDiff (ds, du) (s, u) = diff d i
    where diff a b = fromIntegral $ floor ((a - b) * 1000) -- as millis
          d = asFrac jds jdu
          i = asFrac s u
          jds = (fromJust $ d_get ds) :: Integer
          jdu = (fromJust $ d_get du) :: Integer
          asFrac a b = (fromIntegral a) + ((fromIntegral b) * 0.000001)


-- MIDI Utils
encodeChannel :: (Bits a, Integral a, Integral b) => a -> a -> b
encodeChannel ch cc = fromIntegral (((-) ch 1) .|. cc)

-- MIDI Messages
noteOn :: (Bits a, Integral a, Integral b) => Output -> a -> a -> a -> b -> IO (Maybe IOError)
noteOn o ch val vel t = do
  let evt = makeEvent 0x90 val ch vel t
  sendEvent o evt

noteOff :: (Bits a, Integral a, Integral b) => Output -> a -> a -> b -> IO (Maybe IOError)
noteOff o ch val t = do
  let evt = makeEvent 0x80 val ch 60 t
  sendEvent o evt

makeCtrl :: (Bits a, Integral a, RealFrac b, Integral c) => Output -> a -> C.Param -> b -> c -> IO (Maybe IOError)
makeCtrl o ch (C.CC {C.midi=midi}) n t = makeCC o ch (fromIntegral midi) scaledN t
  where scaledN = floor . (* 127) $ n
makeCtrl o ch (C.NRPN {C.midi=midi, C.range=range}) n t = makeNRPN o ch (fromIntegral midi) scaledN t
  where scaledN = floor . (+ (fromIntegral lowerBound)) . (* ratio) $ n
        ratio = fromIntegral $ upperBound - lowerBound
        (lowerBound, upperBound) = range


-- This is sending CC
makeCC :: (Bits a, Integral a, Integral b) => Output -> a -> a -> a -> b -> IO (Maybe IOError)
makeCC o ch c n t = do
  let evt = makeEvent 0xB0 c ch n t
  sendEvent o evt

-- This is sending NRPN
makeNRPN :: (Bits a, Integral a, Integral b) => Output -> a -> a -> a -> b -> IO (Maybe IOError)
makeNRPN o ch c n t = do
  let nrpn = makeEvent 0xB0
      evts = [nrpn 0x63 ch (shift (c .&. 0x3F80) (-7)) t,
              nrpn 0x62 ch (c .&. 0x7F) t,
              nrpn 0x06 ch (shift (n .&. 0x3F80) (-7)) t,
              nrpn 0x26 ch (n .&. 0x7F) t
             ]
  maybeErrors <- mapM (sendEvent o) evts
  let errors = catMaybes maybeErrors
  case (length errors) of
    0 -> return Nothing
    x -> return $ Just $ userError (show x ++ " errors occurred while sending NRPN Event" ++ show evts)

-- PortMIDI Wrapper

outputDevice :: PM.DeviceID -> Int -> IO (Either Output PM.PMError)
outputDevice deviceID latency = do
  PM.initialize
  result <- PM.openOutput deviceID 1
  case result of
    Left dev ->
      do
        info <- PM.getDeviceInfo deviceID
        putStrLn ("Opened: " ++ show (PM.interface info) ++ ": " ++ show (PM.name info))
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

makeEvent :: (Bits a, Integral a, Integral b) => a -> a -> a -> a -> b -> PM.PMEvent
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
