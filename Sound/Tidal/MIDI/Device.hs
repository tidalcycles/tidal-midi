{-# OPTIONS_HADDOCK not-home #-}
{-|
Convenience functions to access "Sound.PortMidi" devices.
-}
module Sound.Tidal.MIDI.Device where
import qualified Sound.PortMidi as PM

{-| Example usage:

>>> putStrLn =<< displayOutputDevices
ID:	Name
0:	Midi Through Port-0
2:	DSI Tetra MIDI 1

-}
displayOutputDevices :: IO String
displayOutputDevices = do
  devices <- getIndexedDevices
  return $ displayDevices $ getOutputDevices devices

-- | Readable version of a list of indexed MIDI devices 
displayDevices :: Show a => [(a, PM.DeviceInfo)] -> String
displayDevices devices =
  let indices = map (show . fst) devices
      names = map ((":\t"++) . PM.name . snd) devices
      pairs = zipWith (++) indices names
  in unlines ("ID:\tName" : pairs)

getOutputDevices :: [(a, PM.DeviceInfo)] -> [(a, PM.DeviceInfo)]
getOutputDevices = filter (PM.output . snd)

getIndexedDevices :: IO [(Integer, PM.DeviceInfo)]
getIndexedDevices = do
  rawDevices <- getDevices
  return $ zip [0..] rawDevices

getDevices :: IO [PM.DeviceInfo]
getDevices = do
  _ <- PM.initialize
  count <- PM.countDevices
  mapM PM.getDeviceInfo [0..(count - 1)]

getIDForDeviceName :: Num a => String -> IO (Maybe a)
getIDForDeviceName name = do
  odevs <- fmap getOutputDevices getIndexedDevices
  let res = filter (\n -> (PM.name . snd) n == name) odevs
  case res of
    [] -> return Nothing
    [dev] -> return $ Just $ fromIntegral $ fst dev


