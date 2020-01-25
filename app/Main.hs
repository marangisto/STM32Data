{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Main where

import Text.HTML.TagSoup
import System.FilePath
import Data.Monoid
import Data.Char (isSpace)
import Data.List (stripPrefix, break)
import Data.Maybe (fromMaybe)
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

data Pin
    = PowerPin
    { pinName   :: String
    , position  :: Int
    }
    | ResetPin
    { pinName   :: String
    , position  :: Int
    }
    | IOPin
    { pinName   :: String
    , position  :: Int
    , signals   :: [Signal]
    }
    deriving (Show)

data Signal
    = AlternateFunction
    { signalName        :: String
    , alternateFunction :: Int
    }
    | AdditionalFunction
    { signalName        :: String
    }
    deriving (Show)

pinFromTags :: Map.Map String [AltFun] -> [Tag String] -> Pin
pinFromTags pmm (t:ts) = case fromAttrib "Type" t of
    "Power"   -> PowerPin{..}
    "Reset"   -> ResetPin{..}
    "I/O"     -> let altFuns = fromMaybe [] $ Map.lookup pinName pmm
                     altMap = Map.fromList [ (signalName, altFunction) | AltFun{..} <- altFuns ]
                     signals = map (f altMap) $ filter (~=="<Signal>") ts
                  in IOPin{..}
    where pinName = fromAttrib "Name" t
          position = read $ fromAttrib "Position" t
          f altMap t | (Just alternateFunction) <- Map.lookup signalName altMap = AlternateFunction{..}
                     | otherwise = AdditionalFunction{..}
                     where signalName = fromAttrib "Name" t

pinSpecs :: Map.Map String [AltFun] -> String -> [Pin]
pinSpecs pmm
    = map (pinFromTags pmm)
    . partitions (~=="<Pin>")
    . dropWhile (~/="<Pin>")
    . parseTags

stm32CubeMX = "c:/Program Files (x86)/STMicroelectronics/STM32Cube/STM32CubeMX"
dbDir = "db/mcu"
modesXML = "IP/GPIO-STM32L43x_gpio_v1_0_Modes"
mcuXML = "STM32L433R(B-C)Tx"

main :: IO ()
main = do
    pmm <- pinModeMap <$> readFile (stm32CubeMX </> dbDir </> modesXML <.> "xml")
    pins <- pinSpecs pmm <$> readFile (stm32CubeMX </> dbDir </> mcuXML <.> "xml")
    mapM_ print pins

