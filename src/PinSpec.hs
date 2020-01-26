{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module PinSpec (Pin(..), Signal(..), pinSpecs) where

import Text.HTML.TagSoup
import Text.Read (readMaybe)
import Data.Monoid
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import PinMode

type Position = Either String Int

data Pin
    = PowerPin
    { pinName   :: String
    , position  :: Position
    }
    | ResetPin
    { pinName   :: String
    , position  :: Position
    }
    | BootPin
    { pinName   :: String
    , position  :: Position
    }
    | IOPin
    { pinName   :: String
    , position  :: Position
    , signals   :: [Signal]
    }
    | MonoIOPin
    { pinName   :: String
    , position  :: Position
    }
    | NCPin
    { pinName   :: String
    , position  :: Position
    }
    deriving (Show)

data Signal
    = AlternateFunction
    { signalName        :: String
    , alternateFunction :: Int
    }
    | AdditionalFunction
    { signalName        :: String
    }
    deriving (Show)

pinFromTags :: Map.Map String [AltFun] -> [Tag String] -> Pin
pinFromTags pmm (t:ts) = case fromAttrib "Type" t of
    "Power"   -> PowerPin{..}
    "Reset"   -> ResetPin{..}
    "Boot"    -> BootPin{..}
    "I/O"     -> let altFuns = fromMaybe [] $ Map.lookup pinName pmm
                     altMap = Map.fromList [ (signalName, altFunction) | AltFun{..} <- altFuns ]
                     signals = map (f altMap) $ filter (~=="<Signal>") ts
                  in IOPin{..}
    "MonoIO"  -> MonoIOPin{..}
    "NC"      -> NCPin{..}
    _         -> error $ "unexpected: " <> show t
    where pinName = fromAttrib "Name" t
          position = readPosition $ fromAttrib "Position" t
          f altMap t | (Just alternateFunction) <- Map.lookup signalName altMap = AlternateFunction{..}
                     | otherwise = AdditionalFunction{..}
                     where signalName = fromAttrib "Name" t

readPosition :: String -> Position
readPosition s = maybe (Left s) Right $ readMaybe s

pinSpecs :: Map.Map String [AltFun] -> String -> [Pin]
pinSpecs pmm
    = map (pinFromTags pmm)
    . partitions (~=="<Pin>")
    . dropWhile (~/="<Pin>")
    . parseTags

