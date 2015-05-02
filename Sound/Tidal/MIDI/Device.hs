module Sound.Tidal.MIDI.Device where
import qualified Sound.PortMidi as PM
import Control.Exception
import Data.List


withPortMidi = bracket_ PM.initialize PM.terminate

displayOutputDevices = do
  devices <- getIndexedDevices
  return $ displayDevices $ getOutputDevices devices

displayDevices :: Show a => [(a, PM.DeviceInfo)] -> String
displayDevices devices =
  let indices = map (show . fst) devices
      names = map ((":\t"++) . PM.name . snd) devices
      pairs = zipWith (++) indices names
  in unlines (["ID:\tName"]++pairs)

getOutputDevices = filter (PM.output . snd)

getIndexedDevices = do
  rawDevices <- getDevices
  return $ zip [0..] rawDevices

getDevices :: IO ([PM.DeviceInfo])
getDevices = withPortMidi $ do
  count <- PM.countDevices
  mapM PM.getDeviceInfo [0..(count - 1)]


getIDForDeviceName name = do
  odevs <- fmap getOutputDevices getIndexedDevices
  let res = filter (\n -> (PM.name . snd) n == name) odevs
  case res of
    [] -> return Nothing
    [dev] -> return $ Just $ fromIntegral $ fst dev

--main = do
--  putStrLn =<< displayOutputDevices
