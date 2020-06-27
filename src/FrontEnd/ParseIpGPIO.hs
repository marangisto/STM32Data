{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
module FrontEnd.ParseIpGPIO
    ( IpGPIO(..)
    , GPIOPin(..)
    , PinSignal(..)
    , parseIpGPIO
    ) where

import Data.Text (Text, pack, unpack, toUpper)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Utils (fromHex)
import HXT

data IpGPIO = IpGPIO
    { name              :: !Text
    , dbVersion         :: !Text
    , ipType            :: !Text
    , version           :: !Text
    , gpioPins          :: ![GPIOPin]
    } deriving (Show)

data GPIOPin = GPIOPin
    { name              :: !Text
    , portName          :: !Text
    , pinSignals        :: ![PinSignal]
    } deriving (Show)

data PinSignal = PinSignal
    { name              :: !Text
    , gpioAF            :: !Text
    } deriving (Show)

parseIpGPIO :: FilePath -> IO IpGPIO
parseIpGPIO fn = do
    s <- readFile fn
    xs <- runX (readString [ withValidate yes ] s >>> getIpGPIO)
    case xs of
        [x] -> return x
        _ -> error "failed to parse IpGPIO"

getIpGPIO = atTag "IP" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    dbVersion <- arr pack <<< attrText "DBVersion" -< x
    ipType <- arr pack <<< attrText "IPType" -< x
    version <- arr pack <<< attrText "Version" -< x
    gpioPins <- listA getGPIOPin -< x
    returnA -< IpGPIO{..}

getGPIOPin = atTag "GPIO_Pin" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    portName <- arr pack <<< attrText "PortName" -< x
    pinSignals <- listA getPinSignal -< x
    returnA -< GPIOPin{..}

getPinSignal = atTag "PinSignal" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    gpioAF <- getGpioAF `orElse` getRemapBlock -< x
    returnA -< PinSignal{..}

getGpioAF = atTag "SpecificParameter" >>> proc x ->
    arr pack <<< elemText "PossibleValue" -< x

getRemapBlock = atTag "RemapBlock" >>> proc x ->
    arr pack <<< attrText "Name" -< x
