{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Main where

import Text.HTML.TagSoup
import System.FilePath
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
    , alternateFun  :: Int
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
          (alternateFun, peripheral) | Just r <- stripPrefix "GPIO_AF" s
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

data Signal = Signal
    { signalName    :: String
    , alternateFun  :: Maybe Int
    } deriving (Show)

pinFromTags :: [Tag String] -> Pin
pinFromTags (t:ts) = case fromAttrib "Type" t of
    "Power"   -> PowerPin{..}
    "Reset"   -> ResetPin{..}
    "I/O"     -> let signals = map f $ filter (~=="<Signal>") ts
                  in IOPin{..}
    where pinName = fromAttrib "Name" t
          position = read $ fromAttrib "Position" t
          f = uncurry Signal . (,Nothing) . fromAttrib "Name"

stm32CubeMX = "c:/Program Files (x86)/STMicroelectronics/STM32Cube/STM32CubeMX"
dbDir = "db/mcu"
modesXML = "IP/GPIO-STM32L43x_gpio_v1_0_Modes"
mcuXML = "STM32L433R(B-C)Tx"

main :: IO ()
main = do
    pmm <- pinModeMap <$> readFile (stm32CubeMX </> dbDir </> modesXML <.> "xml")
    print pmm
{-
    modes <- parseTags <$> readFile (stm32CubeMX </> dbDir </> modesXML <.> "xml")
    let pins = partitions (~=="<GPIO_Pin>") $ dropWhile (~/="<GPIO_Pin>") modes
    mapM_ (print . pinModeFromTags) pins
-}

    tags <- parseTags <$> readFile (stm32CubeMX </> dbDir </> mcuXML <.> "xml")
    let pins = partitions (~=="<Pin>") $ dropWhile (~/="<Pin>") tags
    mapM_ (print . pinFromTags) pins

