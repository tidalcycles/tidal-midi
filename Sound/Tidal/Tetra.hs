module Sound.Tidal.Tetra where

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
                            F "kcutoff" (Just (164)),
                            F "kresonance" (Just (-1)),
                            F "atk" (Just (-1)),
                            F "dcy" (Just (-1)),
                            F "sus" (Just (-1)),
                            F "rel" (Just (-1)),
                            F "fgain" (Just (-1)),
                            F "fvol" (Just (-1)),
                            F "audiomod" (Just (-1)),
                            F "kamt" (Just (-1)),
                            F "oscmix" (Just (-1)),
                            F "sub1vol" (Just (-1)),
                            F "sub2vol" (Just (-1)),
                            F "noise" (Just (-1)),
                            F "fpoles" (Just (-1)),
                            F "kmode" (Just (-1)),
                            F "ksplitpoint" (Just (-1))
                          ],
                 timestamp = NoStamp,
                 latency = 0,
                 namedParams = False,
                 preamble = []
                }

keyStream = stream "127.0.0.1" 7303 keys

note         = makeI keys "note"

dur = makeF keys "dur"

kcutoff      =  makeF keys "kcutoff"
kresonance      =  makeF keys "kresonance"
atk       = makeF keys "atk"
dcy        = makeF keys "dcy"
sus      = makeF keys "sus"
rel      = makeF keys "rel"
fgain        = makeF keys "fgain"
fvol        = makeF keys "fvol"

audiomod    = makeF keys "audiomod"
kamt      = makeF keys "kamt"

oscmix    = makeF keys "oscmix"
sub1vol   = makeF keys "sub1vol"
sub2vol   = makeF keys "sub2vol"
noise     = makeF keys "noise"


fpoles    = makeF keys "fpoles"
twopole = fpoles (p "0")
fourpole = fpoles (p "1")

kmode     = makeF keys "kmode"
knormal   = kmode (p "0")
kstack   = kmode (p "1")
ksplit   = kmode (p "2")

ksplitpoint   = makeF keys "ksplitpoint"

-- dur          = makeF keys "dur"
-- portamento   = makeF keys "portamento"
-- expression   = makeF keys "expression"
-- octave       = makeF keys "octave"
-- voice        = makeF keys "voice"
-- detune       = makeF keys "detune"
-- vcoegint     = makeF keys "vcoegint"
-- kcutoff      = makeF keys "kcutoff"
-- vcfegint     = makeF keys "vcfegint"
-- lforate      = makeF keys "lforate"
-- lfopitchint  = makeF keys "lfopitchint"
-- lfocutoffint = makeF keys "lfocutoffint"

-- dtime        = makeF keys "dtime"
-- dfeedback    = makeF keys "dfeedback"

paramRanges = [
              164, -- Cutoff, in semitones
              127, -- resonance
              127, -- attack
              127, -- decay
              127, -- sustain
              127, -- release
              127, -- fgain
              127, -- fvol
              127, -- audiomod
              127, -- kamt
              127, -- oscmix
              127, -- sub1vol
              127, -- sub2vol
              127, -- noise
              1, -- fpoles
              2, -- kmode
              127 -- ksplitpoint
              ]

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
     let ctrls' = map floor (zipWith (*) paramRanges ctrls)
         ctrls'' = filter ((>=0) . snd) (zip keynames ctrls')
         --ctrls''' = map (\x -> (x, fromJust $ lookup x ctrls'')) usectrls
     --putStrLn $ show ctrls''
     sequence_ $ map (\(name, ctrl) -> makeCtrl stream channel (ctrlN name ctrl)) ctrls''
     return ()


ctrlN "kcutoff" v         = (15, v)
ctrlN "kresonance" v         = (16, v)

-- filter envelope
ctrlN "atk" v         = (23, v)
ctrlN "dcy" v         = (24, v)
ctrlN "sus" v         = (25, v)
ctrlN "rel" v         = (26, v)

ctrlN "fgain" v         = (110, v)
ctrlN "fvol" v         = (116, v)

ctrlN "audiomod" v         = (18, v)
ctrlN "kamt" v         = (17, v)

ctrlN "oscmix" v         = (13, v)
ctrlN "sub1vol" v         = (114, v)
ctrlN "sub2vol" v         = (115, v)
ctrlN "noise" v         = (14, v)

ctrlN "fpoles" v         = (19, v)

ctrlN "kmode" v         = (119, v)

ctrlN "ksplitpoint" v         = (118, v)

ctrlN s _             = error $ "no match for " ++ s




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
  let nrpn = makeEvent 0xB0
      evts = [nrpn 0x63 ch (shift (c .&. 0x3F80) (-7)) 0,
              nrpn 0x62 ch (c .&. 0x7F)  0,
              nrpn 0x06 ch (shift (n .&. 0x3F80) (-7)) 0,
              nrpn 0x26 ch (n .&. 0x7F) 0
             ]
  maybeErrors <- mapM (sendEvent o) evts
  let errors = catMaybes maybeErrors
  case (length errors) of
    0 -> return Nothing
    x -> return $ Just $ userError (show x ++ " errors occurred while sending NRPN Event" ++ show evts)
