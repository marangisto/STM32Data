{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module IPMode (AltFun(..), altFunMap) where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace)
import Data.List (stripPrefix, break)
import qualified Data.Map.Strict as Map
import MCU (cleanName)

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
    where pinName = cleanName $ fromAttrib "Name" t
          signals = map altFun
                  $ partitions (~=="<PinSignal>")
                  $ dropWhile (~/="<PinSignal>") ts

altFun :: [Tag String] -> AltFun
altFun (t:ts) = AltFun{..}
    where signalName = cleanName $ fromAttrib "Name" t
          (altFunction, peripheral) | Just r <- stripPrefix "GPIO_AF" s
                                    , (u, '_':v) <- break (=='_') r = (read u, v)
                                    | otherwise = error $ "unexpected: '" <> s <> "'"
                 where s = filter (not . isSpace) $ innerText ts

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

