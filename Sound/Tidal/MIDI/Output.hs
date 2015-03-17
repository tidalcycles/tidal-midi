module Sound.Tidal.MIDI.Output where

import qualified Sound.PortMidi as PM

import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Numeric

import Sound.OSC.FD


import Control.Monad
import Control.Concurrent.MVar

import Data.Bits
import Data.Char
import Data.List (sortBy)
import Data.Maybe
import Data.Ord (comparing)

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
                       offset :: (Int, Int),
                       buffer :: MVar [PM.PMEvent],
                       starttime :: UTCTime,
                       midistart :: CULong
                     }


messageLoop stream shape ch port = do
  putStrLn ("Starting message loop on port " ++ show port ++ " for MIDI channel " ++ show ch)
  x <- udpServer "127.0.0.1" port
  -- send defaults to reset controllers

  -- let defaults = map (realToFrac . C.vdefault) (C.params shape)
  -- sendctrls stream shape ch 0 defaults

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


                -- now <- getCurrentTime
                -- putStrLn ("The time is now: " ++ show (floor $ (1000*) $ realToFrac $ diffUTCTime now (starttime stream)))
                -- currentTime <- PM.time
                -- putStrLn (show ch ++ ": at " ++ show usec ++ " - diff " ++ show diff)
                -- putStrLn ("Will play MIDI note in " ++ show (diff - fromIntegral currentTime) ++ "ms (" ++ show (currentTime - (midistart stream)))
                sendmidi stream shape ch (fromIntegral note', realToFrac dur') (diff) ctrls'
                return()


makeStream shape port = S.stream "127.0.0.1" port shape

-- ctrlN s _               = error $ "no match for " ++ s

-- keyproxy :: Bits b => a -> a -> a -> [b]
keyproxy latency deviceID shape channels = do
  let ports = (map (+ 7303) channels)
      keyStreams = map (makeStream (C.toOscShape shape)) ports
  econn <- outputDevice deviceID latency

  case econn of
    Right err -> error ("Failed opening MIDI Output on Device ID: " ++ show deviceID ++ " - " ++ show err)
    Left conn ->
      do
        sendevents conn
        zipWithM_ (messageLoop conn shape) (map fromIntegral channels) ports
        return keyStreams

sendevents stream = do
  forkIO $ do loop stream
    where loop stream = do act stream
                           delay
                           loop stream
          act stream = do
            let evtsM = buffer stream
                o = conn stream
            evts <- takeMVar evtsM
            putMVar evtsM []
            let ecount = length evts
            case ecount of
              0 ->  do
                return Nothing
              x -> do
                -- putStrLn ("Will write " ++ show ecount ++ " events")
                let evts' = sortBy (comparing PM.timestamp) evts
                -- putStrLn ("evts: " ++ show evts')
                err <- PM.writeEvents o evts'
                case err of
                  PM.NoError -> return Nothing
                  e -> return $ Just (userError ("Error '" ++ show e ++ "' sending Events: " ++ show evts))
          delay = threadDelay 40000


sendctrls stream shape ch t ctrls = do
  let ctrls' = filter ((>=0) . snd) (zip (C.toKeynames shape) ctrls)
  sequence_ $ map (\(name, ctrl) -> makeCtrl stream ch (C.paramN shape name) ctrl t) ctrls'
  return ()

sendmidi stream shape ch (note,dur) t ctrls =
  do forkIO $ do noteOn stream ch note 60 t
                 threadDelay $ floor $ (1000000 *) $ (dur) - ((C.latency shape))
                 noteOff stream ch note (t + (floor $ 1000 * dur))
                 return ()
     sendctrls stream shape ch t ctrls
     return ()

-- timeDiff :: Integral a => (Datum, Datum) -> (Int, Int) -> a
timeDiff (ds, du) (s, u) = diff d i
    where diff a b = floor ((a - b) * 1000) -- as millis
          d = asFrac jds jdu
          i = asFrac s u
          jds = (fromJust $ d_get ds) :: Int
          jdu = (fromJust $ d_get du) :: Int
          asFrac a b = (fromIntegral a) + ((fromIntegral b) * 0.000001)


-- MIDI Utils
-- encodeChannel :: (Bits a, Integral a, Integral b) => a -> a -> b
encodeChannel ch cc = (((-) ch 1) .|. cc)

-- MIDI Messages
-- noteOn :: (Bits a, Integral a, Integral b) => Output -> a -> a -> a -> b -> IO (Maybe IOError)
noteOn o ch val vel t = do
  let evt = makeEvent 0x90 val ch vel t
  sendEvent o evt

-- noteOff :: (Bits a, Integral a, Integral b) => Output -> a -> a -> b -> IO (Maybe IOError)
noteOff o ch val t = do
  let evt = makeEvent 0x80 val ch 60 t
  sendEvent o evt

-- makeCtrl :: (Bits a, Integral a, RealFrac b, Integral c) => Output -> a -> C.Param -> b -> c -> IO (Maybe IOError)
makeCtrl o ch (C.CC {C.midi=midi, C.range=range, C.scalef=f}) n t = makeCC o ch (fromIntegral midi) scaledN t
  where scaledN = fromIntegral (f range (n))
-- makeCtrl o ch (C.RPN {C.midi=midi}) n t = makeCC o ch (fromIntegral midi) scaledN t
--   where scaledN = floor n
makeCtrl o ch (C.NRPN {C.midi=midi, C.range=range, C.scalef=f}) n t = makeNRPN o ch (fromIntegral midi) scaledN t
  where scaledN = fromIntegral $ (f range (n))

makeCtrl o ch (C.SysEx {C.midi=midi, C.range=range, C.scalef=f}) n t = makeSysEx o ch (fromIntegral midi) scaledN t
  where scaledN = fromIntegral $ (f range (n))


-- This is sending CC
-- makeCC :: (Bits a, Integral a, Integral b) => Output -> a -> a -> a -> b -> IO (Maybe IOError)
makeCC o ch c n t = do
  let evt = makeEvent 0xB0 c ch n t
  sendEvent o evt

-- This is sending NRPN
-- makeNRPN :: (Bits a, Integral a, Integral b) => Output -> a -> a -> a -> b -> IO (Maybe IOError)
makeNRPN o ch c n t = do
  let nrpn = makeEvent 0xB0
      evts = [nrpn 0x63 ch (shift (c .&. 0x3F80) (-7)) t,
              nrpn 0x62 ch (c .&. 0x7F) t,
              nrpn 0x06 ch (shift (n .&. 0x3F80) (-7)) t,
              nrpn 0x26 ch (n .&. 0x7F) t
             ]
  --maybeErrors <-
  mapM (sendEvent o) evts
  return Nothing
  -- let errors = catMaybes maybeErrors
  -- case (length errors) of
  --   0 -> return Nothing
  --   x -> return $ Just $ userError (show x ++ " errors occurred while sending NRPN Event" ++ show evts)

makeSysEx o ch c n t = do
  let bytes = [0xF0, -- SysEx Start
               0x3E, -- Vendor ID
               0x13, -- Equipment ID
               0x00, -- Device No.
               0x20, -- Message ID
               0x00, -- Location
               shift (c .&. 0x3F80) (-7), -- Parameter Index high byte
               (c .&. 0x7F), -- Parameter Index low byte
               n, -- Parameter Value
               0xF7 -- End of SysEx
              ]

      msg = B.pack $ bytes

  sendSysEx o t (BC.unpack msg)

-- PortMIDI Wrapper

-- outputDevice :: PM.DeviceID -> Int -> IO (Either Output PM.PMError)
outputDevice deviceID latency = do
  PM.initialize
  result <- PM.openOutput deviceID latency
  case result of
    Left dev ->
      do
        info <- PM.getDeviceInfo deviceID
        putStrLn ("Opened: " ++ show (PM.interface info) ++ ": " ++ show (PM.name info))
        sem <- newEmptyMVar
        putMVar sem () -- initially fill MVar to be taken by the first user of this output
        buffer <- newMVar []
        midiOffset <- PM.time -- in milliseconds

        now <- getCurrentTime
        let posixNow = realToFrac $ utcTimeToPOSIXSeconds now
            syncedNow = posixNow - ((0.001*) $ fromIntegral midiOffset)
            sec = floor syncedNow
            usec = floor $ 1000000 * (syncedNow - (realToFrac sec))
        return (Left Output { conn=dev, lock=sem, offset=(sec, usec), starttime=now, midistart=midiOffset, buffer=buffer })
    Right err -> return (Right err)

-- makeEvent :: (Bits a, Integral a, Integral b) => a -> a -> a -> a -> b -> PM.PMEvent
makeEvent st n ch v t = PM.PMEvent msg (t)
  where msg = PM.PMMsg (encodeChannel ch st) (n) (v)

sendSysEx o t msg = do
  let sem = lock o
  takeMVar sem
  err <- PM.writeSysEx (conn o) t msg
  putMVar sem ()
  return Nothing
  -- case err of
  --   PM.NoError -> return Nothing
  --   e -> return $ Just (userError ("Error '" ++ show e ++ "' sending SysEx " ++ show msg ++ "(" ++ show t ++ ")"))

-- now with a semaphore since PortMIDI is NOT thread safe
-- sendEvent :: Output -> PM.PMEvent -> IO (Maybe IOError)
sendEvent o evt = do
  let sem = lock o
      buf = buffer o
  -- takeMVar sem
  cbuf <- takeMVar buf
  putMVar buf (cbuf ++ [evt])
  return Nothing
  -- err <- PM.writeShort (conn o) evt
  -- putMVar sem ()
  -- case err of
  --   PM.NoError -> return Nothing
  --   e -> return $ Just (userError ("Error '" ++ show e ++ "' sending Event: " ++ show evt))
