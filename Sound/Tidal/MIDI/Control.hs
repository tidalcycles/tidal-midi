{- |
Mappings between Tidal's 'Sound.Tidal.Stream.Param's and MIDI control changes
-}
module Sound.Tidal.MIDI.Control where

import qualified Sound.Tidal.Stream as S
import           Sound.Tidal.Tempo (Tempo(cps))
import qualified Data.Map.Strict as Map
import           Data.Ratio
import           Sound.Tidal.Params hiding (n_p)

n_p :: S.Param
n_p = snd $ pI "n" (Just 128)

{-|
Map a 'Double' to 'Int' using given min/max values
-}
type RangeMapFunc = (Int, Int) -> Double -> Int

{- | Make sure you apply @cutShape midiShape@ to an 'Sound.Tidal.Stream.ParamMap'
before passing it into a function wanting this type -}
type MIDINoteShape = S.ParamMap

{-|
Describe mapping a Tidal 'Sound.Tidal.Stream.Param' in terms of MIDI
-}
data ControlChange =
  CC { param :: S.Param, -- ^ the 'Sound.Tidal.Stream.Param' this control will apply to
       midi :: Int, -- ^ the MIDI parameter number to map to
       range :: (Int, Int), -- ^ the range this MIDI parameter accepts, by default this is (0,127)
       scalef :: RangeMapFunc -- ^ the function to apply mapping floating point values from pattern to MIDI integer values
     }
  | NRPN { param :: S.Param,
           midi :: Int,
           range :: (Int, Int),
           scalef :: RangeMapFunc
         }
  | SysEx { param :: S.Param,
            midi :: Int,
            range :: (Int, Int),
            scalef :: RangeMapFunc
          }

{- |
A definition for using a Tidal with specific MIDI device type.

By default, every 'ControllerShape' accepts the following 'Sound.Tidal.Stream.Param's:

* 'Sound.Tidal.Params.dur'
* 'Sound.Tidal.Params.n'
* 'Sound.Tidal.Params.velocity'
* 'Sound.Tidal.Params.nudge'
* 'Sound.Tidal.Params.unit'

which will define the MIDI note to be played.
-}
data ControllerShape = ControllerShape {
  controls :: [ControlChange], -- ^ a list of controls that can be understood by a certain device type
  latency :: Double -- ^ the latency to be used when sending out MIDI messages, this is passed to 'Sound.Tidal.Stream.Shape'
  }

{- |
A simple shape defining the 'Sound.Tidal.Stream.Param's that are used for generating MIDI notes.

This simplifies splitting a 'Sound.Tidal.Stream.ParamMap' into params for notes and control values.
-}
midiShape :: S.Shape
midiShape = S.Shape {
  S.params = [     
     dur_p,
     n_p,
     nudge_p,
     velocity_p,     
     unit_p
     ],
  S.latency = 0,
  S.cpsStamp = False
  }

{- |
Turns a 'MIDINoteShape' into concrete values for scheduling.

-}
computeTiming :: Tempo -- ^ the current playback speed
              -> Ratio Integer {- ^ if 'Sound.Tidal.Params.unit' is specified as @cycle@,
this will be utilized to calculate the note's absolute duration with regard to current cycle length __Note__: this will ignore the specified duration of the Tidal param 'Sound.Tidal.Params.dur' -}
              -> MIDINoteShape -- ^ A map of 'Sound.Tidal.Stream.Param's that describes the note to be played
              -> ((Int,Int,Ratio Integer), Double) -- ^ A tuple of a 'Sound.Tidal.MIDI.Output.TimedNote' triplet and the value for 'nudge' to offset this note by
computeTiming tempo duration m = ((n', v', d'), nudge')
  where
    note' = Map.mapMaybe id m
    unit' = S.svalue $ note' Map.! unit_p    
    v' = mapRange (0, 127) $ S.fvalue $ note' Map.! velocity_p
    n' = S.ivalue $  note' Map.! n_p
    d' = case unit' of
      "rate" -> byRate
      "cycle" -> (+) (-0.001) $ (/) duration $ realToFrac $ cps tempo
      _ -> byRate
    byRate = realToFrac $ S.fvalue $ note' Map.! dur_p

    nudge' = S.fvalue $ note' Map.! nudge_p

{- | Converts a 'ControllerShape's controls into 'Sound.Tidal.Stream.Param's and makes a 'Sound.Tidal.Stream.Shape'
This acts as an interface between Tidal's scheduling loop and MIDI scheduling. -}
toShape :: ControllerShape -> S.Shape
toShape cs = S.Shape {
  S.params = toParams cs,
  S.cpsStamp = False,
  S.latency = latency cs
  }

{- | A 'RangeMapFunc' that simply passes 'floor's 'Double's.

This can be used if a MIDI parameter of a device has different meanings for each value,
e.g. the type of oscillator has to be specified by either "0", "1", "2" or "3" each representing a different waveform (sine, tri, square, rand)
-}
passThru :: (Int, Int) -> Double -> Int
passThru (_, _) = floor -- no sanitizing of range…

{- | Default mapping function from Double to Int.

>>> mapRange (0, 127) 0.5
63
-}
mapRange :: (Int, Int) -> Double -> Int
mapRange (low, high) = floor . (+ fromIntegral low) . (* ratio)
  where ratio = fromIntegral $ high - low

-- | Helper function for creating a standard ControlChange for MIDI parameter
mCC :: S.Param -> Int -> ControlChange
mCC p m = CC {param=p, midi=m, range=(0, 127), scalef=mapRange }

-- | Helper function for creating a standard ControlChange for a non-registered MIDI parameter
mNRPN :: S.Param -> Int -> ControlChange
mNRPN p m = NRPN {param=p, midi=m, range=(0, 127), scalef=mapRange }

-- | Helper function for creating a ControlChange for a non-registered MIDI parameter with a custom range
mrNRPN :: S.Param -> Int -> (Int, Int) -> ControlChange
mrNRPN p m r = NRPN {param=p, midi=m, range=r, scalef=mapRange }

-- | Translate a 'ControllerShape's controls into a list of 'Sound.Tidal.Stream.Param'
toParams :: ControllerShape -> [S.Param]
toParams shape' = map param (controls shape')

{- | Translate a Tidal 'Sound.Tidal.Stream.Param' into the corresponding MIDI parameter number
according to a specific 'ControllerShape' -}
ctrlN :: Num b => ControllerShape -> S.Param -> Maybe b
ctrlN shape' x = (fromIntegral . midi) <$> (paramN shape' x)

-- | Find the first 'ControlChange' that uses 'Sound.Tidal.Stream.Param'
paramN :: ControllerShape -> S.Param -> Maybe ControlChange
paramN shape' x
  | x `elem` names = paramX $ matching p
  | otherwise = Nothing
  where names = toParams shape'
        paramX [] = Nothing
        paramX (h:_) = Just h
        matching = filter ((== x) . param)
        p = controls shape'
