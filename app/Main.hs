{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Main where

import Data.Char (toLower)
import Data.List (stripPrefix)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs hiding (name)
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Control.Monad.Extra
import Family as F
import IPMode
import MCU
import Pretty

type Text = T.Text

data Options = Options
    { list_mcus     :: Bool
    , alt_fun       :: Maybe FilePath
    , build_rules   :: Bool
    , family_header :: Bool
    , family        :: [String]
    , sub_family    :: [String]
    , package       :: [String]
    , files         :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { list_mcus = def &= help "list available MCUs by family"
    , alt_fun = def &= help "generate alternate function header(s)"
    , build_rules = def &= help "generate source for build rules"
    , family_header = def &= help "generate family header"
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
        fs = map (Family . T.pack) family ++ map (SubFamily . T.pack) sub_family ++ map (Package . T.pack) package
    families <- prune fs . parseFamilies <$> T.readFile (dbDir </> familiesXML)
    when list_mcus $ mcuList families
    when build_rules $ mapM_ (putStrLn . T.unpack) $ buildRules families
    when family_header $ forM_ families $ \(family, subFamilies) -> do
        let cs = controllers subFamilies
        mapM_ (putStrLn . T.unpack) $ familyHeader $ map refName cs
        forM_ cs $ \c -> do
            mcu@MCU{..} <- loadMCU dbDir c
            print mcu{pins = []}-- gpioConfig
        xs <- gpioConfigs dbDir family
        ys <- forM xs $ \x -> (x,) <$> gpioConfigSet dbDir x
        mapM_ (putStrLn . T.unpack) $ gpioConfigHeader ys

identFromRefName :: String -> String
identFromRefName s
    | ('x' : p : _ : rest) <- reverse s = reverse $ p : 'x' : rest
    | (_ : 'x' : p : _ : rest) <- reverse s = reverse $ p : 'x' : rest
    | otherwise = error $ "unrecognized name format: '" <> s <> "'"

components :: String -> (Char, Char, Char, Char)
components s
    | (Just rest) <- stripPrefix "STM32" s
    , [ _, _, c1, c2, c3, 'x', c4 ] <- rest
    = (c1, c2, c3, c4)
    | otherwise = error $ "unrecognized name format: '" <> s <> "'"

