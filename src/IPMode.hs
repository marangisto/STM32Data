{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module IPMode
    ( AltFun(..)
    , GPIOConf(..)
    , PIN(..)
    , AF(..)
    , loadMCU
    , gpioConfigSet
    , gpioConfigs
    ) where

import System.FilePath
import System.Directory
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (stripPrefix, isPrefixOf, break)
import Data.List.Extra (stripSuffix)
import Control.Arrow
import TagSoup
import Family
import MCU

newtype GPIOConf = GPIOConf { unGPIOConf :: Text } deriving (Eq, Ord, Show)
newtype PIN = PIN { unPIN :: Text } deriving (Eq, Ord, Show)
newtype AF = AF { unAF :: Text } deriving (Eq, Ord, Show)

data IPMode = IPMode
    { pinName   :: Text
    , signals   :: [AltFun]
    }
    deriving (Show)

data AltFun = AltFun
    { signalName    :: Text
    , altFunction   :: Int
    , peripheral    :: Text
    }
    deriving (Show)

pinModeFromTags :: [Tag Text] -> IPMode
pinModeFromTags (t:ts) = IPMode{..}
    where pinName = cleanPin $ fromAttrib "Name" t
          signals = mapMaybe altFun
                  $ partitions (~=="<PinSignal>")
                  $ dropWhile (~/="<PinSignal>") ts

altFun :: [Tag Text] -> Maybe AltFun
altFun (t:ts)
    | Just r <- T.stripPrefix "GPIO_AF" s
    , (u, peripheral) <- second (T.drop 1) $ T.breakOn "_" r
    = let altFunction = read $ T.unpack u
       in Just AltFun{..}
    | (u:_) <- filter (~=="<RemapBlock>") ts
    , fromAttrib "DefaultRemap" u == "true"
    = let altFunction = 0
          peripheral = fromAttrib "Name" u  -- FIXME: do we even care about snd?
       in Just AltFun{..}
    | otherwise = Nothing                   -- FIXME: remapping not yet supported
    where signalName = cleanSignal $ fromAttrib "Name" t
          s = T.strip $ innerText ts

altFunSet :: Text -> Set.Set ((PIN, AF), Int)
altFunSet xml = Set.fromList
    [ ((PIN pinName, AF signalName), altFunction)
    | IPMode{..} <- xs
    , AltFun{..} <- signals
    ]
    where xs = map pinModeFromTags
             $ partitions (~=="<GPIO_Pin>")
             $ dropWhile (~/="<GPIO_Pin>")
             $ parseTags xml

altFunMap :: Text -> Map.Map (Text, Text) Int
altFunMap xml = Map.fromList
    [ ((pinName, signalName), altFunction)
    | IPMode{..} <- xs
    , AltFun{..} <- signals
    ]
    where xs = map pinModeFromTags
             $ partitions (~=="<GPIO_Pin>")
             $ dropWhile (~/="<GPIO_Pin>")
             $ parseTags xml

loadMCU :: FilePath -> Controller -> IO MCU
loadMCU dbDir c@Controller{..} = do
    mcu@MCU{..} <- parseMCU c <$> T.readFile (dbDir </> T.unpack name <.> "xml")
    afmap <- altFunMap <$> T.readFile (dbDir </> "IP" </> "GPIO-" <> T.unpack gpioConfig <> "_Modes" <.> "xml")
    return $ mcu { pins = map (resolveFunctions afmap) pins }

gpioConfigSet :: FilePath -> Text -> IO (Set.Set ((PIN, AF), Int))
gpioConfigSet dbDir gpioConfig = altFunSet <$> T.readFile (dbDir </> "IP" </> "GPIO-" <> T.unpack gpioConfig <> "_Modes" <.> "xml")

gpioConfigs :: FilePath -> Text -> IO [Text]
gpioConfigs dbDir family' = do
    map T.pack . mapMaybe (extractGpioConfig $ T.unpack family') <$> getDirectoryContents (dbDir </> "IP")

extractGpioConfig :: String -> FilePath -> Maybe String
extractGpioConfig family s0
    | Just (s1, s2) <- extractGpioConfig' s0 = case family of
          "STM32L4" | family `isPrefixOf` s1 -> if s1 /= "STM32L4P" then Just s2 else Nothing
          "STM32L4+" | s1 == "STM32L4P" -> Just s2
          "STM32MP1" | s1 == "STM32MPU" -> Just s2
          _ -> if family `isPrefixOf` s1 then Just s2 else Nothing
    | otherwise = Nothing

extractGpioConfig' :: FilePath -> Maybe (String, String)
extractGpioConfig' s0
    | Just s1 <- stripPrefix "GPIO-" s0
    , (s2, _) <- break (=='_') s1
    , Just s3 <- stripSuffix "_Modes.xml" s1 = Just (s2, s3)
    | otherwise = Nothing

