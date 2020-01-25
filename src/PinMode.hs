{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module PinMode (AltFun(..), pinModeMap) where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace)
import Data.List (stripPrefix, break)
import qualified Data.Map.Strict as Map

data PinMode = PinMode
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

pinModeFromTags :: [Tag String] -> PinMode
pinModeFromTags (t:ts) = PinMode{..}
    where pinName = fromAttrib "Name" t
          signals = map altFun
                  $ partitions (~=="<PinSignal>")
                  $ dropWhile (~/="<PinSignal>") ts

altFun :: [Tag String] -> AltFun
altFun (t:ts) = AltFun{..}
    where signalName = fromAttrib "Name" t
          (altFunction, peripheral) | Just r <- stripPrefix "GPIO_AF" s
                                    , (u, '_':v) <- break (=='_') r = (read u, v)
                                    | otherwise = error "failed to extract alternate function"
                 where s = filter (not . isSpace) $ innerText ts

pinModeMap :: String -> Map.Map String [AltFun]
pinModeMap xml = Map.fromList [ (pinName, signals) | PinMode{..} <- xs ]
    where xs = map pinModeFromTags
             $ partitions (~=="<GPIO_Pin>")
             $ dropWhile (~/="<GPIO_Pin>")
             $ parseTags xml

