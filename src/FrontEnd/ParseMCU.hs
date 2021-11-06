{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FrontEnd.ParseMCU
    ( MCU(..)
    , IP(..)
    , Pin(..)
    , Sig(..)
    , parseMCU
    ) where

import Data.Text (Text, pack, unpack, isPrefixOf, stripSuffix, intercalate)
import Text.Read (readMaybe)
import HXT

data MCU = MCU
    { refName           :: !Text
    , svd               :: !Text
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
    , signals           :: ![Sig]
    } deriving (Show, Eq, Ord)

data Sig = Sig
    { name              :: !Text
    , ioModes           :: !(Maybe Text)
    } deriving (Show, Eq, Ord)

parseMCU :: [Text] -> FilePath -> IO MCU
parseMCU svds fn = do
    s <- readFile fn
    xs <- runX (readString [ withValidate yes ] s >>> getMCU svds)
    case xs of
        [x] -> return x
        _ -> error "failed to parse MCU"

getMCU :: ArrowXml cat => [Text] -> cat (NTree XNode) MCU
getMCU svds = atTag "Mcu" >>> proc x -> do
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
    returnA -< MCU{svd=matchSVD svds refName,..}

getIP :: ArrowXml cat => cat (NTree XNode) IP
getIP = atTag "IP" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    instanceName <- arr pack <<< attrText "InstanceName" -< x
    version <- arr pack <<< attrText "Version" -< x
    configFile <- arr (fmap pack) <<< attrTextMay "ConfigFile" -< x
    clockEnableMode <- arr (fmap pack) <<< attrTextMay "ClockEnableMode" -< x
    returnA -< IP{..}

getPin :: ArrowXml cat => cat (NTree XNode) Pin
getPin = atTag "Pin" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    position <- arr readPosition <<< attrText "Position" -< x
    type_ <- arr pack <<< attrText "Type" -< x
    signals <- listA getSignal -< x
    returnA -< Pin{..}

getSignal :: ArrowXml cat => cat (NTree XNode) Sig
getSignal = atTag "Signal" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    ioModes <- arr (fmap pack) <<< attrTextMay "IOModes" -< x
    returnA -< Sig{..}

readBool :: String -> Bool
readBool "true" = True
readBool "false" = False
readBool s = error $ "bad bool: '" <> s <> "'"

readPosition :: String -> Either Text Int
readPosition s = maybe (Left $ pack s) Right $ readMaybe s

matchSVD :: [Text] -> Text -> Text
matchSVD svds name = case filter p svds of
    [] | "STM32F105" `isPrefixOf` name -> "STM32F107"
    [] | "STM32F205" `isPrefixOf` name -> "STM32F215"
    [] | "STM32F207" `isPrefixOf` name -> "STM32F217"
    [] | "STM32F415" `isPrefixOf` name -> "STM32F405"
    [] | "STM32F417" `isPrefixOf` name -> "STM32F407"
    [] | "STM32F423" `isPrefixOf` name -> "STM32F413"
    [] | "STM32F437" `isPrefixOf` name -> "STM32F427"
    [] | "STM32F439" `isPrefixOf` name -> "STM32F429"
    [] | "STM32F479" `isPrefixOf` name -> "STM32F469"
    [] | "STM32H725" `isPrefixOf` name -> "STM32H73x"
    [] | "STM32H735" `isPrefixOf` name -> "STM32H73x"
    [] | "STM32L486" `isPrefixOf` name -> "STM32L476"
    [] | "STM32L4A6" `isPrefixOf` name -> "STM32L476"
    [] | "STM32WB5M" `isPrefixOf` name -> "STM32WB55" -- FIXME: questionable!
    [] | "STM32WLE4" `isPrefixOf` name -> "STM32WLE5_CM4"
    [] ->  error $ "failed to match svd: " <> unpack
                (name <> " <> " <> intercalate "," svds)
    [ svd ] -> svd
    xs -> case filter (notElem 'x' . unpack) xs of
        [ svd ] -> svd
        _ -> error $ unpack name <> " matches " <> show xs
    where p = all match . zip (unpack name) . unpack . chooseM4
          match (n, s) = n == s || s == 'x' || n == 'x'
          chooseM4 svd | Just x <- stripSuffix "_CM4" svd = x
                       | otherwise = svd
