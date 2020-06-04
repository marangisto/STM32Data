{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
module ParseMCU
    ( MCU(..)
    , IP(..)
    , Pin(..)
    , Signal(..)
    , parseMCU
    ) where

import Data.Text (Text, pack, unpack, toUpper)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Utils (fromHex)
import HXT

data MCU = MCU
    { refName           :: !Text
    , dbVersion         :: !Text
    , clockTree         :: !Text
    , family            :: !Text
    , line              :: !Text
    , package           :: !Text
    , hasPowerPad       :: !Bool
    , die               :: !Text
    , ips               :: ![IP]
    , pins              :: ![Pin]
    } deriving (Show)

data IP = IP
    { name              :: !Text
    , instanceName      :: !Text
    , version           :: !Text
    , configFile        :: !(Maybe Text)
    , clockEnableMode   :: !(Maybe Text)
    } deriving (Show)

data Pin = Pin
    { name              :: !Text
    , position          :: !(Either Text Int)
    , type_             :: !Text
    , signals           :: ![Signal]
    } deriving (Show)

data Signal = Signal
    { name              :: !Text
    , ioModes           :: !(Maybe Text)
    } deriving (Show)

parseMCU :: FilePath -> IO MCU
parseMCU fn = do
    s <- readFile fn
    xs <- runX (readString [ withValidate yes ] s >>> getMCU)
    case xs of
        [x] -> return x
        _ -> error "failed to parse MCU"

getMCU = atTag "Mcu" >>> proc x -> do
    refName <- arr pack <<< attrText "RefName" -< x
    dbVersion <- arr pack <<< attrText "DBVersion" -< x
    clockTree <- arr pack <<< attrText "ClockTree" -< x
    family <- arr pack <<< attrText "Family" -< x
    line <- arr pack <<< attrText "Line" -< x
    package <- arr pack <<< attrText "Package" -< x
    hasPowerPad <- arr readBool <<< attrText "HasPowerPad" -< x
    die <- arr pack <<< elemText "Die" -< x
    -- skip core, freq, etc as they are already in family data
    ips <- listA getIP -< x
    pins <- listA getPin -< x
    returnA -< MCU{..}

getIP = atTag "IP" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    instanceName <- arr pack <<< attrText "InstanceName" -< x
    version <- arr pack <<< attrText "Version" -< x
    configFile <- arr (fmap pack) <<< attrTextMay "ConfigFile" -< x
    clockEnableMode <- arr (fmap pack) <<< attrTextMay "ClockEnableMode" -< x
    returnA -< IP{..}

getPin = atTag "Pin" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    position <- arr readPosition <<< attrText "Position" -< x
    type_ <- arr pack <<< attrText "Type" -< x
    signals <- listA getSignal -< x
    returnA -< Pin{..}

getSignal = atTag "Signal" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    ioModes <- arr (fmap pack) <<< attrTextMay "IOModes" -< x
    returnA -< Signal{..}

readBool :: String -> Bool
readBool "true" = True
readBool "false" = False
readBool s = error $ "bad bool: '" <> s <> "'"

readPosition :: String -> Either Text Int
readPosition s = maybe (Left $ pack s) Right $ readMaybe s

