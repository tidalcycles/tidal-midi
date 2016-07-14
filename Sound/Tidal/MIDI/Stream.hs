{-|
Entry functions for interacting with MIDI devices through Tidal.
-}
module Sound.Tidal.MIDI.Stream (midiStream, midiBackend, midiState, midiSetters, midiDevices, displayOutputDevices) where

-- generics
import           Control.Concurrent
import           Control.Concurrent.MVar ()
import qualified Data.Map as Map

-- Tidal specific
import           Sound.Tidal.Stream as S
import           Sound.Tidal.Time
import           Sound.Tidal.Transition (transition)

-- MIDI specific
import           Sound.Tidal.MIDI.Control
import           Sound.Tidal.MIDI.Output


{-|
Create a handle for all currently used 'Output's indexed by their device name.

We use this to cache once opened devices.

This will be passed to _every_ initialization of a virtual stream to a MIDI device
and is necessary since, 'PortMidi' only allows a single connection to a device.
-}
midiDevices :: IO (MVar MidiDeviceMap)
midiDevices = newMVar $ Map.fromList []


{-|
Connect to a MIDI device with a given name and channel,
using a 'ControllerShape' to allow customized interaction
with specific MIDI synths.

Needs a 'MidiDeviceMap' to operate, create on using 'midiDevices'!

Usage:

@
(m1, mt1) <- midiSetters devices "My Synth Controller Device name" 1 synthController getNow
@

To find the correct name for your device see 'displayOutputDevices'
-}
midiSetters :: MVar MidiDeviceMap -- ^ A list of MIDI output devices
            -> String -- ^ The name of the output device to connect
            -> Int -- ^ The MIDI Channel to use
            -> ControllerShape -- ^ The definition of params to be usable
            -> IO Time -- ^ a method to get the current time
            -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
midiSetters d n c s getNow = do
  ds <- midiState d n c s
  return (setter ds, transition getNow ds)


{-|
Creates a single virtual stream to a MIDI device using a specific 'ControllerShape'

Needs a 'MidiDeviceMap' to operate, create one using 'midiDevices'!
-}
midiStream :: MVar MidiDeviceMap -> String -> Int -> ControllerShape -> IO (ParamPattern -> IO ())
midiStream d n c s = do
  backend <- midiBackend d n c s
  stream backend (toShape s)

{-|
Creates a single virtual state for a MIDI device using a specific 'ControllerShape'

This state can be used to either create a 'Sound.Tidal.Stream.setter' or a 'Sound.Tidal.Transition.transition' from it.

Needs a 'MidiDeviceMap' to operate, create one using 'midiDevices'!
-}
midiState :: MVar MidiDeviceMap -> String -> Int -> ControllerShape -> IO (MVar (ParamPattern, [ParamPattern]))
midiState d n c s = do
  backend <- midiBackend d n c s
  S.state backend (toShape s)


{-|
Opens a connection to a MIDI device and wraps it in a 'Sound.Tidal.Stream.Backend' implementation.

Needs a 'MidiDeviceMap' to operate, create one using 'midiDevices'!
-}
midiBackend :: MVar MidiDeviceMap -> String -> Int -> ControllerShape -> IO (Backend a)
midiBackend d n c cs = do
  (s, o) <- makeConnection d n c cs
  return $ Backend s (flushBackend o)

