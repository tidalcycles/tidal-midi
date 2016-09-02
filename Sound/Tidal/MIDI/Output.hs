{-# OPTIONS_HADDOCK not-home #-}
{-|
A bridge between evaluated Tidal patterns and MIDI events.

This module contains functions necessary to mediate between
'Sound.Tidal.Time.Event's generated from a Tidal 'Sound.Tidal.Pattern.Pattern'
and plain MIDI events sent through 'Sound.PortMidi.PMStream'.
-}
module Sound.Tidal.MIDI.Output (
  -- * Types
  Output(..),
  OutputState,
  MidiDeviceMap,
  TimedNote,
  -- * Initialization
  makeConnection,
  flushBackend,
  -- * Scheduling
  sendevents,
  store,
  mkStore,
  storeParams,
  scheduleTime,
  -- * Converters
  toMidiValue,
  cutShape,
  stripDefaults,
  -- * State handling
  changeState,
  readState,
  -- * Low-level functions
  useOutput,
  displayOutputDevices,
  outputDevice,
  makeRawEvent,
  noteOn,
  noteOff,
  makeCtrl
                               ) where

-- generics
import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.MVar ()
import           Data.Bits
import           Data.List (sortBy, find, partition)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Ratio (Ratio)
import           Data.Time (getCurrentTime, UTCTime)
import           Data.Time.Clock.POSIX
import           Foreign.C
import           Numeric

-- Tidal specific
import           Sound.Tidal.Tempo (Tempo(Tempo))
import           Sound.Tidal.Stream as S

-- MIDI specific
import           Sound.Tidal.MIDI.Device
import           Sound.Tidal.MIDI.Control
import qualified Sound.PortMidi as PM

type ConnectionCount = Int
type TickedConnectionCount = Int
type OutputOnline = Bool
{- |
Keep track of virtual streams

* Reflects the number of virtual streams that have already stored their events for this tick. Every time 'TickedConnectionCount' cycles, MIDI events will be sent out.
* 'ConnectionCount' is increased on every new stream created via `midiSetters`
* For each channel, currently used params and their values are kept.
* Output will only be scheduling, once __online__, i.e. when the first stream is initialized
-}
type OutputState = (
  TickedConnectionCount, 
  ConnectionCount,
  [ParamMap], 
  OutputOnline 
  )

type Tick = Int
type Onset = Double
type Offset = Double
type RelativeOffset = Double
type MIDITime = (Tempo, Tick, Onset, RelativeOffset)
type MIDIEvent = (MIDITime, MIDIMessage)
type MIDIChannel = CLong
type MIDIStatus = CLong
type MIDINote = MIDIDatum
type MIDIVelocity = MIDIDatum
type MIDIDatum = CLong
type MIDIDuration = Ratio Integer
type MIDIMessage = (MIDIChannel, MIDIStatus, MIDINote, MIDIVelocity)
-- | A Triplet of the deviation from the note @a5@, velocity and duration
type TimedNote = (CLong, MIDIVelocity, MIDIDuration)
type SentEvent = (CULong, Double, PM.PMEvent, CULong, UTCTime)
{-|
An abstract definition of a physical MIDI Output.

Manages virtual streams to multiple channels of a single connection to a MIDI device.
-}
data Output = Output {
  cshape :: ControllerShape, -- ^ The ControllerShape defining which 'Param's will be available for use
  conn :: PM.PMStream, -- ^ The physical connection to the device, uses 'PortMidi'
  buffer :: MVar ([ParamMap], [MIDIEvent]), -- ^ A buffer of currently used 'Param's and their 'Value's as well as a list of 'MIDIEvent's to be sent on the next tick.
  bufferstate :: MVar OutputState, -- ^ Keeps track of connected virtual streams during one tick
  midistart :: CULong, -- ^ the MIDI time when this output was created
  rstart :: UTCTime -- ^ the real time when this output was created
  }

type MidiMap = Map.Map S.Param (Maybe Int)
type MidiDeviceMap = Map.Map String Output


-- | Initialize a connection to the given MIDI device by Name
makeConnection :: MVar MidiDeviceMap -- ^ The current list of already connected devices
               -> String -- ^ The MIDI device name
               -> Int -- ^ The MIDI channel
               -> ControllerShape -- ^ The definition of useable 'Control's
               -> IO (S.ToMessageFunc, Output) -- ^ A function to schedule MIDI events and the output that keeps track of connections
makeConnection devicesM displayname channel controllershape = do
  moutput <- useOutput devicesM displayname controllershape
  case moutput of
    Just o -> do
      s <- connected channel displayname o
      return (s, o)
    Nothing ->
      error "Failed initializing MIDI connection"


{- |
Sends out MIDI events once all virtual streams have buffered their events.

This will be called after every tick
-}
flushBackend :: Output -> S.Shape -> Tempo -> Int -> IO ()
flushBackend o shape change ticks = do
  changeState tickConnections o
  cycling <- readState isCycling o

  Control.Monad.when cycling (do
      -- gather last sent params, update state with new
      let buf = buffer o
      (states, events) <- takeMVar buf

      ((_,_,newstates,_), (_,_,oldstates,_)) <- changeState' (resetParamStates states) o

      -- find params that were removed
      let mapDefaults = Map.mapWithKey (\k _ -> defaultValue k)
          diffs = map mapDefaults $ zipWith Map.difference oldstates newstates
      -- store additional "reset" events in buffer
      -- schedule time must be exactly before/ontime with the next regular event to be sent. otherwise we risk
      -- mixing order of ctrl messages, and resets get overridden
      -- FIXME: when scheduling, note late CC messages and DROP THEM, otherwise everything is screwed
      let offset = S.latency shape
          mididiffs = map ((toMidiMap (cshape o)).(stripShape (toShape $ cshape o))) $ diffs          
          resetevents = concat $ zipWith (\x y -> makectrls o x (change,ticks,1,offset) y) [1..] mididiffs

      -- send out MIDI events
      (late, later) <- sendevents o shape change ticks events resetevents
      -- finally clear buffered ParamMap for next tick
      putMVar buf (replicate 16 Map.empty, later)
      let len = length late
      case len of
        0 ->
          return ()
        _ -> do
          putStrLn $ showLate $ head late
          putStrLn $ "and " ++ show (len - 1) ++ " more")


-- Scheduling

{- |
Sends out MIDI events due for this tick.
-}
sendevents :: Output -- ^ The connection to be used
           -> S.Shape -- ^ The shape to be queried for latency
           -> Tempo -- ^ The current speed
           -> Tick -- ^ The number of ticks elapsed since start, may be reset when using @cps (-1)@
           -> [MIDIEvent] -- ^ A list of events potentially needed to be sent
           -> [MIDIEvent] -- ^ A list of reset events potentially needed to be sent
           -> IO ([SentEvent], [MIDIEvent]) -- ^ A list of events sent late and a list of events to send later
sendevents _ _ _ _ [] [] = return ([],[])
sendevents s shape change ticks evts resets = do
  -- assumptions:
  -- all reset events have the same timestamp
  -- questions:      
  -- could there be any events in `evts` at all that need reset? or are these just in late from the last tick?
  let output = conn s
      toDescriptor midiTime now (o,_,t,e) = (o,t,e, midiTime, now)
      calcOnsets (a@(tempo, tick, onset, offset), e) = (a, logicalOnset' tempo tick onset offset, e)

  midiTime <- PM.time
  now <- getCurrentTime
  let offset = S.latency shape
      nextTick = logicalOnset' change (ticks+1) 0 offset
      mkEvent (t, o, e) = (midionset, t, o, makeRawEvent e midionset)
        where midionset = scheduleTime (midistart s, rstart s) o     
      onsets = map calcOnsets evts
      -- calculate temporary scheduling for resetevts
      resetevts = map calcOnsets resets
      -- split into events sent now and later (e.g. a noteOff that would otherwise cut off noteOn's in the next tick)
      (evts', later) = span ((< nextTick).(\(_,o,_) -> o)) $ sortBy (comparing (\(_,o,_) -> o)) onsets
      -- calculate MIDI time to schedule events, putting time into fn to create PM.PMEvents
      evts'' = map mkEvent evts'
      -- a list CC `names` that need to be reset
      resetccs = map (\(_, _, (_, _, d1, _)) -> d1) resetevts
      later' = map (\(t,_,e) -> (t,e)) later
      findCC match list = find (\(_, _, (_, st, d1, _)) -> st == 0xB0 && (d1 `elem` match)) $ reverse list


      -- 1. find the ccs that needs reset (search in `later` then in `evts`)
      (evtstosend, laterevts) = case findCC resetccs later of
        Nothing -> case findCC resetccs evts' of
          -- 1c. no events at all need to be reset
          --      1cI. use the default passed in midionset for resets
          --      1cII. append `resets` to `evts` FIXME: make sure we really do by timing
          --      1cIII. send `evts`
          Nothing -> (evts'' ++ map mkEvent resetevts, later')
          -- 1b. only `evts` contain a CC to be reset
          --      1bI. set scheduletime for reset __after__ the latest CC that needs to be reset in `evts`
          --      1bII. add `resets` to `evts`
          --      1bIII. send `evts`                
          Just (_, latestO, _) -> (before ++
                                      map (
                                          \(t, o, e) ->
                                          let midionset = scheduleTime (midistart s, rstart s) latestO
                                          in (midionset, t,o,makeRawEvent e midionset)
                                          ) resetevts ++ after, later')
            where
              (before, after) = partition (\(m,_,o,_) -> m > scheduleTime (midistart s, rstart s) o) evts''

        -- 1a. `later` contains a cc to be reset, (omit searching in evts)
        --      1aI. set scheduletime for reset __after__ the latest CC that needs to be reset in `later`
        --      1aII. add `resetevts` to `later`
        --      1aIII. send `evts`                
        Just (latestT, _, _) -> (evts'', later' ++ map (\(_, _, e) -> (latestT, e)) resetevts)
      evtstosend' = map (\(_,_,_,e) -> e) evtstosend
      -- filter events that are too late
      late = map (toDescriptor midiTime now) $ filter (\(_,_,t,_) -> t < realToFrac (utcTimeToPOSIXSeconds now)) evtstosend
      -- drop late CC events to avoid glitches
--      evtstosend'' = map (\(_,_,e,_,_) -> e) $ filter (not.isCC) late
  -- write events for this tick to stream
  err <- PM.writeEvents output evtstosend'

  case err of
   PM.NoError -> return (late, laterevts)  -- return events for logging in outer scope
   e -> do
     putStrLn ("sending failed: " ++ show e)
     return (late, laterevts)

isCC :: SentEvent -> Bool
isCC (_,_,e,_,_) = (0x0f .&. cc) == 0xB0
  where
    cc = PM.status $ PM.decodeMsg $ PM.message $ e


    
-- | Buffer a single tick's MIDI events for a single channel of a single connection 
store :: Output -> Int -> Tempo -> Tick -> Onset -> Offset -> MidiMap -> ParamMap -> IO ()
store s ch change tick on off ctrls note = storemidi s ch' note' (change, tick, on, offset) ctrls
    where
      (note', nudge) = computeTiming' change on off note
      ch' = fromIntegral ch
      cshape' = cshape s
      offset = Sound.Tidal.MIDI.Control.latency cshape' + nudge

{- |
Returns a function to be called on every tick,
splits the given @ParamMap@ into MIDI note information
and CCs.
-}
mkStore :: Int -> Output -> IO ToMessageFunc
mkStore channel s = return $ \ shape change tick (on,off,m) -> do
                        let ctrls = cutShape shape m
                            props = cutShape midiShape m
                            ctrls' = stripDefaults ctrls
                            ctrls'' = toMidiMap (cshape s) <$> ctrls'
                            store' = store s channel change tick on off <$> ctrls''
                        -- store even non-midi params, otherwise removing last ctrl results in a missing reset since diff in `flushBackend` would be empty
                        -- then buffer ctrl messages to be sent
                        -- with the appropriate note properties
                        ($) <$> (storeParams s channel <$> stripDefaults (applyShape' shape m)) <*> (($) <$> store' <*> props)


-- | Union the currently stored paramstate for certain channel with the given one
storeParams :: Output -> Int -> ParamMap -> IO () -> IO ()
storeParams o ch m action = do
  modifyMVar_ (buffer o) $ \(states, events) -> do
    let (before,current:after) = splitAt (ch - 1) states
        state' = Map.union m current
        states' = before ++ [state'] ++ after
    return (states', events)
  action

-- | Thin wrapper around @computeTiming@ to convert onset/offset into onset/duration relative
computeTiming' :: Tempo -> Double -> Double -> ParamMap -> (TimedNote, Double)
computeTiming' tempo on off note = ((fromIntegral n, fromIntegral v, d), nudge)
  where
    ((n,v,d), nudge) = computeTiming tempo (realToFrac (off - on) / S.ticksPerCycle) note

{- | Schedule sending all CC's default values.
Produces an `onTick` handler.
-}
connected :: Int -> String -> Output -> IO ToMessageFunc
connected channel displayname s = do
  let cshape' = cshape s
      shape = toShape $ cshape s
      defaultParams = S.defaultMap shape
      allctrls = toMidiMap cshape' defaultParams
  putStrLn ("Successfully initialized Device '" ++ displayname ++ "'")
  changeState goOnline s
  now <- getCurrentTime
  _ <- storeevents s $ makectrls s (fromIntegral channel) (Tempo now 0 1 False 0,0,0,0) allctrls
  mkStore channel s

-- State handling

readState :: (OutputState -> b) -> Output -> IO b
readState f o = do
  s <- readMVar $ bufferstate o

  return $ f s

isCycling :: OutputState -> Bool
isCycling (0, _, _, True) = True
isCycling _ = False

-- displayState :: OutputState -> String
-- displayState (ticked, conns, paramstate, online) = show ticked ++ "/" ++ show conns ++ "[" ++ show online ++ "]" ++ " active params: " ++ show paramstate

changeState :: (OutputState -> OutputState) -> Output -> IO ()
changeState f o = do
  _ <- changeState' f o
  return ()

changeState' :: (OutputState -> OutputState) -> Output -> IO (OutputState, OutputState)
changeState' f o = do
  bs <- takeMVar stateM
  let fs = f bs
  putMVar stateM fs
  return (fs, bs)
    where
      stateM = bufferstate o

-- | Params in use get overwritten by new ones, except if new ones means _no params_, in this case keep old
resetParamStates :: [ParamMap] -> OutputState -> OutputState
resetParamStates newstates (ticked, conns, paramstates, online) = (ticked, conns, zipWith resetParamState newstates paramstates, online)

resetParamState :: ParamMap -> ParamMap -> ParamMap
resetParamState newstate currentstate
  | Map.empty == newstate = currentstate -- updating with an empty state is a noop
  | otherwise = newstate

goOnline :: OutputState -> OutputState
goOnline (ticked, conns, paramstate, _) = (ticked, conns, paramstate, True)

addConnection :: OutputState -> OutputState
addConnection (ticked, conns, paramstate, online) = (ticked, conns + 1, paramstate, online)

tickConnections :: OutputState -> OutputState
tickConnections (ticked, conns, paramstate, online) = ((ticked + 1) `mod` conns, conns, paramstate, online)

-- | open named MIDI output or use cached (PortMIDI doesn't like opening two connections to the same device!)
useOutput :: MVar MidiDeviceMap -> String -> ControllerShape -> IO (Maybe Output)
useOutput outsM displayname controllershape = do
  outs <- readMVar outsM -- blocks
  let outM = Map.lookup displayname outs -- maybe
  -- if we have a valid output by now, return
  case outM of
    Just o -> do
      putStrLn "Cached Device Output"
      changeState addConnection o -- blocks
      return $ Just o
    Nothing -> do
      -- otherwise open a new output and store the result in the mvar
      devidM <- (>>= maybe (failed displayname "Failed opening MIDI Output Device ID") return) (getIDForDeviceName displayname)
      econn <- outputDevice devidM 1 controllershape  -- either
      case econn of
        Left o -> do
          changeState addConnection o
          _ <- swapMVar outsM $ Map.insert displayname o outs
          return $ Just o
        Right _ -> return Nothing


-- | Turn logicalOnset into MIDITime
scheduleTime :: (CULong, UTCTime)-> Double -> CULong
scheduleTime (mstart', rstart') logicalOnset = (+) mstart $ floor $ 1000 * (logicalOnset - rstart'')
  where
    rstart'' = realToFrac $ utcTimeToPOSIXSeconds rstart'
    mstart = fromIntegral mstart'
    
-- Converters

{-|
Convert a @Param@'s @Value@ into a MIDI consumable datum.

Applies range mapping and scaling functions according to @ControllerShape@
-}
toMidiValue :: ControllerShape -> S.Param -> Value -> Maybe Int
toMidiValue s p (VF x) = ($) <$> mscale <*> mrange <*> pure x
    where
      mrange = fmap range mcc
      mscale = fmap scalef mcc
      mcc = paramN s p
toMidiValue _ _ (VI x) = Just x
toMidiValue _ _ (VS _) = Nothing -- ignore strings for now, we might 'read' them later

-- | Translates generic params into midi params
toMidiMap :: ControllerShape -> S.ParamMap -> MidiMap
toMidiMap s m = Map.mapWithKey (toMidiValue s) (Map.mapMaybe id m)

-- | Keep only params that are in a given shape, replace missing with defaults
cutShape :: S.Shape -> ParamMap -> Maybe ParamMap
cutShape s m = flip Map.intersection (S.defaultMap s) <$> S.applyShape' s m

-- | Keep only params that are in a given shape
stripShape :: S.Shape -> ParamMap -> ParamMap
stripShape s = Map.intersection p'
  where
    p' = S.defaultMap s

-- | Keep only params that are explicitly set (i.e. not default)
stripDefaults :: Maybe ParamMap -> Maybe ParamMap
stripDefaults m = Map.filterWithKey (\k v -> v /= defaultValue k) <$> m


-- Event creation

-- FIXME: throws if param cannot be found
makectrls :: Output -> MIDIChannel -> MIDITime -> MidiMap -> [MIDIEvent]
makectrls o ch t ctrls = concatMap (\(param', ctrl) -> makeCtrl ch (fromJust $ paramN shape param') (fromIntegral ctrl) t) ctrls'
  where
    shape = cshape o
    ctrls' = filter ((>=0) . snd) $ Map.toList $ Map.mapMaybe id ctrls

makenote :: MIDIChannel -> TimedNote -> MIDITime -> [MIDIEvent]
makenote ch (note,vel,dur) (tempo,tick,onset,offset) = noteon' ++ noteoff'
  where
    noteon' = noteOn ch midinote vel (tempo,tick,onset,offset)
    noteoff' = noteOff ch midinote (tempo,tick,onset,offset + fromRational dur)
    midinote = note + 60

makemidi :: Output -> MIDIChannel -> TimedNote -> MIDITime -> MidiMap -> [MIDIEvent]
makemidi o ch (128,_,_) t ctrls = makectrls o ch t ctrls -- HACK: to send only CC use (n + 60) == 128
makemidi o ch note t ctrls = makectrls o ch t ctrls ++ makenote ch note t

-- Event buffering
storemidi :: Output -> MIDIChannel -> TimedNote -> MIDITime -> MidiMap -> IO ()
storemidi o ch n t ctrls = do
  _ <- storeevents o $ makemidi o ch n t ctrls
  return ()


makeEvent :: MIDIStatus -> MIDINote -> MIDIChannel -> MIDIVelocity -> MIDITime -> MIDIEvent
makeEvent st n ch v t = (t, msg)
  where
    msg = (ch, st, n, v)

storeevents :: Output -> [MIDIEvent] -> IO (Maybe a)
storeevents o evts = do
  let buf = buffer o
  (paramstate, cbuf) <- takeMVar buf
  putMVar buf (paramstate, cbuf ++ evts)
  return Nothing

-- Misc helpers

showLate :: SentEvent -> String
showLate (o, t, e, m, n) =
  unwords ["late",
           show $ (\x -> [PM.status x, PM.data1 x, PM.data2 x]) $ PM.decodeMsg $ PM.message e,
           "midi now ", show m, " midi onset: ", show o,
           "onset (relative): ", show $ showFFloat (Just 3) (t - realToFrac (utcTimeToPOSIXSeconds n)) "",
           ", sched: ", show $ PM.timestamp e]

showEvent :: PM.PMEvent -> String
showEvent e = show t ++ " " ++ show msg
  where msg = PM.decodeMsg $ PM.message e
        t = PM.timestamp e

showRawEvent :: (CULong, MIDITime, Double, PM.PMEvent) -> String
showRawEvent (_, (_,_,onset,offset), logicalOnset, e) = "(" ++ show onset ++ "," ++ show offset ++ ") / " ++ show logicalOnset ++ " " ++  showEvent e

failed :: (Show a, Show b) => a -> b -> c
failed di err = error (show err ++ ": " ++ show di)


---------------
-- LOW LEVEL --
---------------

-- MIDI Event wrapping
makeRawEvent :: MIDIMessage -> CULong -> PM.PMEvent
makeRawEvent (ch, st, n, v) = PM.PMEvent msg
  where msg = PM.encodeMsg $ PM.PMMsg (encodeChannel ch st) n v


-- MIDI Utils
encodeChannel :: MIDIChannel -> MIDIStatus -> CLong
encodeChannel ch cc = (-) ch 1 .|. cc


-- MIDI Messages
noteOn :: MIDIChannel -> MIDINote -> MIDIVelocity -> MIDITime -> [MIDIEvent]
noteOn ch val vel t = [makeEvent 0x90 val ch vel t]

noteOff :: MIDIChannel -> MIDINote -> MIDITime -> [MIDIEvent]
noteOff ch val t = [makeEvent 0x80 val ch 60 t]

makeCtrl :: MIDIChannel -> ControlChange -> MIDIDatum -> MIDITime -> [MIDIEvent]
makeCtrl ch CC {midi=midi'} n t = makeCC ch (fromIntegral midi') n t -- FIXME: no SysEx support right now
makeCtrl ch NRPN {midi=midi'} n t = makeNRPN ch (fromIntegral midi') n t

makeCC :: MIDIChannel -> MIDIDatum -> MIDIDatum -> MIDITime -> [MIDIEvent]
makeCC ch c n t = [makeEvent 0xB0 c ch n t]

makeNRPN :: MIDIChannel -> MIDIDatum -> MIDIDatum -> MIDITime -> [MIDIEvent]
makeNRPN ch c n t = [
  nrpn 0x63 ch (shift (c .&. 0x3F80) (-7)) t,
  nrpn 0x62 ch (c .&. 0x7F) t,
  nrpn 0x06 ch (shift (n .&. 0x3F80) (-7)) t,
  nrpn 0x26 ch (n .&. 0x7F) t
  ]
  where
    nrpn = makeEvent 0xB0


-- | Creates an 'Output' wrapping a PortMidi device
outputDevice :: PM.DeviceID -> Int -> ControllerShape -> IO (Either Output PM.PMError)
outputDevice deviceID latency' shape = do
  _ <- PM.initialize
  result <- PM.openOutput deviceID latency'
  bs <- newMVar (0, 0, replicate 16 Map.empty, False)
  case result of
    Left dev ->
      do
        info <- PM.getDeviceInfo deviceID
        time <- getCurrentTime        
        mstart <- PM.time        
        putStrLn ("Opened: " ++ show (PM.interface info) ++ ": " ++ show (PM.name info))
        b <- newMVar (replicate 16 Map.empty, [])

        return (Left Output { cshape=shape, conn=dev, buffer=b, bufferstate=bs, midistart=mstart, rstart=time })
    Right err -> return (Right err)
