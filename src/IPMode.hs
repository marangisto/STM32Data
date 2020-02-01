{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields, OverloadedStrings #-}
module IPMode (AltFun(..), loadMCU) where

import System.FilePath
import Data.Monoid
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow
import TagSoup
import MCU

data IPMode = IPMode
    { pinName   :: !Text
    , signals   :: ![AltFun]
    }
    deriving (Show)

data AltFun = AltFun
    { signalName    :: !Text
    , altFunction   :: !Int
    , peripheral    :: !Text
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

loadMCU :: FilePath -> Text -> IO MCU
loadMCU dbDir name = do
    mcu@MCU{..} <- parseMCU <$> T.readFile (dbDir </> T.unpack name <.> "xml")
    afmap <- altFunMap <$> T.readFile (dbDir </> "IP" </> "GPIO-" <> T.unpack gpioConfig <> "_Modes" <.> "xml")
    return $ mcu { pins = map (resolveFunctions afmap) pins }

