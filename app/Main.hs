{-# LANGUAGE DeriveDataTypeable, TupleSections, RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs hiding (name, enum)
import System.FilePath
import System.Directory
import System.IO
import System.IO.Temp
import System.Info
import Control.Monad
import Control.Monad.Extra
import Data.List (nub, sort, isPrefixOf)
import Data.List.Extra (groupSort)
import Data.Maybe (fromMaybe)
import Data.Hashable
import FrontEnd.Families
import FrontEnd
import PrettyCPP
import KiCadSymbol
import Utils

data Options = Options
    { list_mcus     :: Bool
    , build_rules   :: Bool
    , recache       :: Bool
    , clock_control :: Bool
    , kicad_symbol  :: Maybe Text
    , headers       :: Maybe FilePath
    , family        :: [String]
    , sub_family    :: [String]
    , package       :: [String]
    , files         :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { list_mcus = def &= help "list available MCUs by family"
    , build_rules = def &= help "generate source for build rules"
    , recache = def &= help "force file cache refresh"
    , clock_control = def &= help "report missing and unused clock control"
    , kicad_symbol = def &= help "generate KiCad symbol for MCU"
    , family = def &= help "filter on family"
    , sub_family = def &= help "filter on sub-family"
    , package = def &= help "filter on package"
    , headers = def &= help "generate headers (to directory)"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate pin descriptions from STM32CubeMX xml files" &=
    summary "STM32Data v0.4.0, (c) Bengt Marten Agren 2020" &=
    details [ "STM32Data generate device header files for STM32"
            , "MCUs based on vendor XML files from SMT32CubeMX."
            ]

tmpDir :: FilePath
tmpDir = "C:" </> "tmp"

famDir = case os of
    "linux" -> "/usr/local/STMicroelectronics/STM32Cube"
    "darwin" -> "/Applications/STMicroelectronics/STM32CubeMX.app"
    _       -> "C:/Program Files/STMicroelectronics/STM32Cube"

svdDir = case os of
    "linux" -> "/opt/st/stm32cubeide_1.3.0"
    "darwin" -> "/Applications/STM32CubeIDE.app"
    _       -> "C:/ST/STM32CubeIDE_1.7.0"

main :: IO ()
main = do
    Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    let fs = concat
            [ map (OnFamily . T.pack) family
            , map (OnSubFamily . T.pack) sub_family
            , map (OnPackage . T.pack) package
            ]
    [famXML] <- cacheLines recache familiesFile famDir
    allFamilies <- parseFamilies famXML
    let families = prune fs allFamilies
    allSVDs <- cacheLines recache svdFiles svdDir

    let dbDir = takeDirectory famXML

    whenJust headers $ \outDir -> do
        prettyFamiliesCPP outDir $ map (unPlus . fst) allFamilies
        forM_ families $ \(family', subs) -> do
            x <- processFamily svdDir dbDir recache family' subs
            prettyFamilyCPP outDir x

    when list_mcus $ mcuList families
    when build_rules
        $ mapM_ (putStrLn . T.unpack)
        $ buildRules families

    whenJust kicad_symbol $ \name -> do
        forM_ families $ \(family', subs) -> do
            mcu <- singleMCU svdDir dbDir recache family' subs name
            kiCadSymbol name mcu

svdFiles :: FilePath -> IO [FilePath]
svdFiles = traverseDir (const True) accept []
    where accept :: [FilePath] -> FilePath -> IO [FilePath]
          accept xs fp
            | takeExtension fp == ".svd" = return $ fp : xs
            | otherwise = return xs

familiesFile :: FilePath -> IO [FilePath]
familiesFile = traverseDir (const True) accept []
    where accept :: [FilePath] -> FilePath -> IO [FilePath]
          accept xs fp
            | takeFileName fp == "families.xml" = return $ fp : xs
            | otherwise = return xs
