{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module MCU
    ( MCU(..)
    , Pin(..)
    , Signal(..)
    , parseMCU
    , resolveFunctions
    , alternateFunctions
    , cleanName
    ) where

import Text.HTML.TagSoup
import Text.Read (readMaybe)
import Data.Monoid
import Data.List (sort)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

data MCU = MCU
    { refName       :: String
    , clockTree     :: String
    , family        :: String
    , line          :: String
    , package       :: String
    , powerPad      :: Bool
    , gpioConfig    :: String
    , pins          :: [Pin]
    } deriving (Show)

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
    = Unresolved
    { signalName        :: String
    }
    | AlternateFunction
    { signalName        :: String
    , alternateFunction :: Int
    }
    | AdditionalFunction
    { signalName        :: String
    }
    deriving (Show)

pinFromTags :: [Tag String] -> Pin
pinFromTags (t:ts) = case fromAttrib "Type" t of
    "Power"   -> PowerPin{..}
    "Reset"   -> ResetPin{..}
    "Boot"    -> BootPin{..}
    "I/O"     -> let signals = map (Unresolved . cleanName . fromAttrib "Name")
                             $ filter (~=="<Signal>") ts
                  in IOPin{..}
    "MonoIO"  -> MonoIOPin{..}
    "NC"      -> NCPin{..}
    _         -> error $ "unexpected: " <> show t
    where pinName = cleanName $ fromAttrib "Name" t
          position = readPosition $ fromAttrib "Position" t

cleanName :: String -> String
cleanName = map (\c -> if c == '-' then '_' else c)

readPosition :: String -> Position
readPosition s = maybe (Left s) Right $ readMaybe s

resolveFunctions :: Map.Map (String, String) Int -> Pin -> Pin
resolveFunctions af p@IOPin{..} = p { signals = map f signals }
    where f Unresolved{..}
              | (Just alternateFunction) <- Map.lookup (pinName, signalName) af = AlternateFunction{..}
              | otherwise = AdditionalFunction{..}
resolveFunctions _ p = p

parseMCU :: String -> MCU
parseMCU xml = MCU{..}
    where refName = fromAttrib "RefName" t
          clockTree = fromAttrib "ClockTree" t
          family = fromAttrib "Family" t
          line = fromAttrib "Line" t
          package = fromAttrib "Package" t
          powerPad = fromAttrib "HasPowerPad" t == "true"
          gpioConfig = getConfig "GPIO" ts
          pins = map pinFromTags . partitions (~=="<Pin>") $ dropWhile (~/="<Pin>") ts
          (t:ts) = dropWhile (~/="<Mcu>") $ parseTags xml

getConfig :: String -> [Tag String] -> String
getConfig name ts
    | (t:_) <- filter p ts = fromAttrib "Version" t
    | otherwise = error $ "failed to get config for " <> name
    where p t = t ~=="<IP>" && fromAttrib "Name" t == name

alternateFunctions :: [Pin] -> [(String, String, Int)]
alternateFunctions pins = sort
    [ (pinName, signalName, alternateFunction)
    | IOPin{..} <- pins
    , AlternateFunction{..} <- signals
    ]

