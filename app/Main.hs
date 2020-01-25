{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Main where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace)
import Data.List (stripPrefix, break, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import PinMode
import PinSpec
import Family

data Options = Options
    { modeFile  :: Maybe FilePath
    , files     :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { modeFile = def &= help "pin-mode file for MCU (required)"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate pin descriptions from STM32CubeMX xml files" &=
    summary "STM32Data v0.0.0, (c) Bengt Marten Agren 2020" &=
    details [ "STM32Data generate device header files for STM32"
            , "MCUs based on vendor XML files from SMT32CubeMX."
            ]

stm32CubeMX = "c:/Program Files (x86)/STMicroelectronics/STM32Cube/STM32CubeMX"
dbDir = "db/mcu"
familiesXML = "families.xml"
modesXML = "IP/GPIO-STM32L43x_gpio_v1_0_Modes"
mcuXML = "STM32L433R(B-C)Tx"

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    families <- parseFamilies <$> readFile (stm32CubeMX </> dbDir </> familiesXML)
    forM_ families $ \(name, subFamilies) -> do
        putStrLn $ replicate 80 '='
        putStrLn name
        putStrLn $ replicate 80 '='
        forM_ subFamilies $ \(name, mcus) -> do
            putStrLn $ replicate 80 '-'
            putStrLn $ "    " <> name
            putStrLn $ replicate 80 '-'
            forM_ mcus $ \MCU{..} -> do
                putStrLn $ "        " <> unwords [ name, package, refName, rpn, show flash <> "/" <> show ram ]
 


    {-
    let pred s = "STM32" `isPrefixOf` s && ".xml" `isSuffixOf` s
    xs <- filter pred <$> listDirectory (stm32CubeMX </> dbDir)
    forM_ xs $ \x -> do
        putStrLn x
    -}
    {-
    pmm <- pinModeMap <$> readFile (fromMaybe (error "--mode-file requied") modeFile)
    forM_ files $ \file -> do
        pins <- pinSpecs pmm <$> readFile file
        mapM_ print pins
        -}

