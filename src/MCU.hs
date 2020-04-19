{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields, OverloadedStrings #-}
module MCU
    ( MCU(..)
    , Pin(..)
    , Signal(..)
    , parseMCU
    , resolveFunctions
    , alternateFunctions
    , cleanPin
    , cleanSignal
    ) where

import Family
import Text.Read (readMaybe)
import Data.List (sort, break)
import Data.Char (isAlphaNum)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import TagSoup

data MCU = MCU
    { name          :: Text                 -- from Controller
    , core          :: Text                 -- from Controller
    , frequency     :: Int                  -- from Controller
    , flash         :: Int                  -- from Controller
    , ram           :: Int                  -- from Controller
    , numIO         :: Int                  -- from Controller
    , peripherals   :: [(Peripheral, Int)]  -- from Controller
    , clockTree     :: Text
    , family        :: Text
    , line          :: Text
    , package       :: Text
    , powerPad      :: Bool
    , gpioConfig    :: Text
    , pins          :: [Pin]
    } deriving (Eq, Ord, Show)

type Position = Either Text Int

data Pin
    = PowerPin
    { pinName   :: Text
    , position  :: Position
    }
    | ResetPin
    { pinName   :: Text
    , position  :: Position
    }
    | BootPin
    { pinName   :: Text
    , position  :: Position
    }
    | IOPin
    { pinName   :: Text
    , position  :: Position
    , signals   :: [Signal]
    }
    | MonoIOPin
    { pinName   :: Text
    , position  :: Position
    }
    | NCPin
    { pinName   :: Text
    , position  :: Position
    }
    deriving (Eq, Ord, Show)

data Signal
    = Unresolved
    { signalName        :: Text
    }
    | AlternateFunction
    { signalName        :: Text
    , alternateFunction :: Int
    }
    | AdditionalFunction
    { signalName        :: Text
    }
    deriving (Eq, Ord, Show)

signalFromTag :: Tag Text -> Maybe Signal
signalFromTag t = Unresolved . cleanSignal <$> case fromAttrib "Name" t of
    "GPIO" -> if "EVENTOUT" `T.isInfixOf` fromAttrib "IOModes" t
                  then Just "EVENTOUT"
                  else Nothing
    name   -> Just name

pinFromTags :: [Tag Text] -> Pin
pinFromTags (t:ts) = case fromAttrib "Type" t of
    "Power"   -> PowerPin{..}
    "Reset"   -> ResetPin{..}
    "Boot"    -> BootPin{..}
    "I/O"     -> let signals = mapMaybe signalFromTag $ filter (~=="<Signal>") ts in IOPin{..}
    "MonoIO"  -> MonoIOPin{..}
    "NC"      -> NCPin{..}
    _         -> error $ "unexpected: " <> show t
    where pinName = cleanPin $ fromAttrib "Name" t
          position = readPosition $ fromAttrib "Position" t

cleanPin :: Text -> Text
cleanPin = T.pack . fst . break (not . isAlphaNum) . T.unpack -- FIXME: capture what we throw away in another field?

cleanSignal :: Text -> Text
cleanSignal = T.pack . map (\c -> if c == '-' then '_' else c) . T.unpack

readPosition :: Text -> Position
readPosition s = maybe (Left s) Right $ readMaybe (T.unpack s)

resolveFunctions :: Map.Map (Text, Text) Int -> Pin -> Pin
resolveFunctions af p@IOPin{..} = p { signals = map f signals }
    where f Unresolved{..}
              | (Just alternateFunction) <- Map.lookup (pinName, signalName) af = AlternateFunction{..}
              | otherwise = AdditionalFunction{..}
resolveFunctions _ p = p

parseMCU :: Controller -> Text -> MCU
parseMCU Controller{..} xml = MCU{..}
    where name = refName -- from family file
          clockTree = fromAttrib "ClockTree" t
          family = fromAttrib "Family" t
          line = fromAttrib "Line" t
          package = fromAttrib "Package" t
          powerPad = fromAttrib "HasPowerPad" t == "true"
          gpioConfig = getConfig "GPIO" ts
          pins = map pinFromTags . partitions (~=="<Pin>") $ dropWhile (~/="<Pin>") ts
          (t:ts) = dropWhile (~/="<Mcu>") $ parseTags xml

getConfig :: Text -> [Tag Text] -> Text
getConfig name ts
    | (t:_) <- filter p ts = fromAttrib "Version" t
    | otherwise = error $ "failed to get config for " <> T.unpack name
    where p t = t ~=="<IP>" && fromAttrib "Name" t == name

alternateFunctions :: [Pin] -> [(Text, Text, Int)]
alternateFunctions pins = sort
    [ (pinName, signalName, alternateFunction)
    | IOPin{..} <- pins
    , AlternateFunction{..} <- signals
    ]

