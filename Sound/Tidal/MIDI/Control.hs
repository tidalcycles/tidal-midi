module Sound.Tidal.MIDI.Control where

import qualified Sound.Tidal.Stream as S

data Param = CC { name :: String, midi :: Int }
           | RPN { name :: String, midi :: Int } -- passes through value as Integer (using floor)
           | NRPN { name :: String, midi :: Int, range :: (Int, Int) }
           | SysEx { name :: String, midi :: Int, range :: (Int, Int) }

data ControllerShape = ControllerShape {params :: [Param],duration :: (String, Double), latency :: Double}

toOscShape :: ControllerShape -> S.OscShape
toOscShape cs =
  let oscparams = [S.I "note" Nothing] ++ [S.F durn (Just durv)] ++ oscparams'
      oscparams' = [S.F (name p) (Just (-1)) | p <- (params cs)]
      (durn, durv) = duration cs
  in S.OscShape {S.path = "/note",
                 S.params = oscparams,
                 S.cpsStamp = False,
                 S.timestamp = S.MessageStamp,
                 S.latency = latency cs,
                 S.namedParams = False,
                 S.preamble = []
                }


toKeynames :: ControllerShape -> [String]
toKeynames shape = map name (params shape)

ctrlN shape x = fromIntegral $ midi $ paramN shape x

paramN shape x
  | x `elem` names = paramX x
  | otherwise = error $ "No such Controller param: " ++ show x
  where names = toKeynames shape
        paramX x = head paramX'
        paramX' = filter ((== x) . name) p
        p = params shape
