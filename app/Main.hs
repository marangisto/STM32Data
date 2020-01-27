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
import Control.Monad.Extra
import PinMode
import PinSpec
import Family as F
import AltFun as AltFun

mcuList :: [Family] -> IO ()
mcuList families = 
    forM_ families $ \(name, subFamilies) -> do
        putStrLn $ replicate 80 '='
        putStrLn name
        forM_ subFamilies $ \(name, mcus) -> do
            putStrLn $ replicate 80 '-'
            putStrLn $ "    " <> name
            putStrLn $ replicate 80 '-'
            forM_ mcus $ \F.MCU{..} -> do
                putStrLn $ "        " <> unwords [ name, package, refName, rpn, show flash <> "/" <> show ram ]

data Options = Options
    { list_mcus     :: Bool
    , alt_fun       :: Maybe FilePath
    , family        :: [String]
    , sub_family    :: [String]
    , package       :: [String]
    , files         :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { list_mcus = def &= help "list available MCUs by family"
    , alt_fun = def &= help "generate alternate function header(s)"
    , family = def &= help "filter on family"
    , sub_family = def &= help "filter on sub-family"
    , package = def &= help "filter on package"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate pin descriptions from STM32CubeMX xml files" &=
    summary "STM32Data v0.0.0, (c) Bengt Marten Agren 2020" &=
    details [ "STM32Data generate device header files for STM32"
            , "MCUs based on vendor XML files from SMT32CubeMX."
            ]

stm32CubeMX = "c:/Program Files (x86)/STMicroelectronics/STM32Cube/STM32CubeMX"
stm32DbDir = "db/mcu"
familiesXML = "families.xml"
modesXML = "IP/GPIO-STM32L43x_gpio_v1_0_Modes"
mcuXML = "STM32L433R(B-C)Tx"

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    let dbDir = stm32CubeMX </> stm32DbDir
        fs = map Family family ++ map SubFamily sub_family ++ map Package package
    --mcu <- parseMCU <$> readFile (dbDir </> mcuXML <.> "xml")
    --print mcu

    families <- prune fs . parseFamilies <$> readFile (dbDir </> familiesXML)
    when list_mcus $ mcuList families
    whenJust alt_fun $ \outputDir -> mapM_ (AltFun.pretty dbDir outputDir) $ concatMap snd $ concatMap snd families


{-
    let pred s = "STM32" `isPrefixOf` s && ".xml" `isSuffixOf` s
    xs <- map takeBaseName . filter pred <$> listDirectory (stm32CubeMX </> dbDir)
    forM_ xs $ \x -> do
        putStrLn x
-}
    {-
    pmm <- pinModeMap <$> readFile (fromMaybe (error "--mode-file requied") modeFile)
    forM_ files $ \file -> do
        pins <- pinSpecs pmm <$> readFile file
        mapM_ print pins
        -}

