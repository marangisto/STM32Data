{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module PinSpec (Pin(..), Signal(..), pinSpecs) where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import PinMode

data Pin
    = PowerPin
    { pinName   :: String
    , position  :: Int
    }
    | ResetPin
    { pinName   :: String
    , position  :: Int
    }
    | IOPin
    { pinName   :: String
    , position  :: Int
    , signals   :: [Signal]
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
    "I/O"     -> let altFuns = fromMaybe [] $ Map.lookup pinName pmm
                     altMap = Map.fromList [ (signalName, altFunction) | AltFun{..} <- altFuns ]
                     signals = map (f altMap) $ filter (~=="<Signal>") ts
                  in IOPin{..}
    where pinName = fromAttrib "Name" t
          position = read $ fromAttrib "Position" t
          f altMap t | (Just alternateFunction) <- Map.lookup signalName altMap = AlternateFunction{..}
                     | otherwise = AdditionalFunction{..}
                     where signalName = fromAttrib "Name" t

pinSpecs :: Map.Map String [AltFun] -> String -> [Pin]
pinSpecs pmm
    = map (pinFromTags pmm)
    . partitions (~=="<Pin>")
    . dropWhile (~/="<Pin>")
    . parseTags

