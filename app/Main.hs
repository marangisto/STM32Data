{-# LANGUAGE RecordWildCards, TupleSections #-}
module Main where

import Text.HTML.TagSoup
import System.FilePath
import Data.Monoid

data PinType = Power | Reset | I_O deriving (Show)

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
    tags <- parseTags <$> readFile (stm32CubeMX </> dbDir </> mcuXML <.> "xml")
    let pins = partitions (~=="<Pin>") $ dropWhile (~/="<Pin>") tags
    mapM_ (print . pinFromTags) pins

