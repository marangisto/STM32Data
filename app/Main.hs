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
import IPMode
import MCU
import Family as F
import AltFun as AltFun

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

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    let dbDir = stm32CubeMX </> stm32DbDir
        fs = map Family family ++ map SubFamily sub_family ++ map Package package
    families <- prune fs . parseFamilies <$> readFile (dbDir </> familiesXML)
    when list_mcus $ mcuList families
    whenJust alt_fun $ \outputDir -> mapM_ (AltFun.pretty dbDir outputDir) $ concatMap snd $ concatMap snd families

