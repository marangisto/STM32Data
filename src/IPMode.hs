{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module IPMode (AltFun(..), altFunMap) where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace)
import Data.List (stripPrefix, break)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import MCU (cleanPin, cleanSignal)

data IPMode = IPMode
    { pinName   :: String
    , signals   :: [AltFun]
    }
    deriving (Show)

data AltFun = AltFun
    { signalName    :: String
    , altFunction   :: Int
    , peripheral    :: String
    }
    deriving (Show)

pinModeFromTags :: [Tag String] -> IPMode
pinModeFromTags (t:ts) = IPMode{..}
    where pinName = cleanPin $ fromAttrib "Name" t
          signals = mapMaybe altFun
                  $ partitions (~=="<PinSignal>")
                  $ dropWhile (~/="<PinSignal>") ts

altFun :: [Tag String] -> Maybe AltFun
altFun (t:ts)
    | Just r <- stripPrefix "GPIO_AF" s
    , (u, '_':peripheral) <- break (=='_') r
    = let altFunction = read u
       in Just AltFun{..}
    | (u:_) <- filter (~=="<RemapBlock>") ts
    , fromAttrib "DefaultRemap" u == "true"
    = let altFunction = 0
          peripheral = fromAttrib "Name" u  -- FIXME: do we even care about snd?
       in Just AltFun{..}
    | otherwise = Nothing                   -- FIXME: remapping not yet supported
    where signalName = cleanSignal $ fromAttrib "Name" t
          s = filter (not . isSpace) $ innerText ts

altFunMap :: String -> Map.Map (String, String) Int
altFunMap xml = Map.fromList
    [ ((pinName, signalName), altFunction)
    | IPMode{..} <- xs
    , AltFun{..} <- signals
    ]
    where xs = map pinModeFromTags
             $ partitions (~=="<GPIO_Pin>")
             $ dropWhile (~/="<GPIO_Pin>")
             $ parseTags xml

