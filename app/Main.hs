{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs hiding (name)
import System.FilePath
import System.Directory
import System.IO
import Control.Monad
import Control.Monad.Extra
import Data.List (isPrefixOf)
import Family as F
import IPMode
import Pretty
import ParseSVD
import PrettySVD
import NormalSVD

type Text = T.Text

data Options = Options
    { list_mcus     :: Bool
    , build_rules   :: Bool
    , family_header :: Maybe FilePath
    , family        :: [String]
    , sub_family    :: [String]
    , package       :: [String]
    , parse_svd     :: Bool
    , address_map   :: Bool
    , normal_svd    :: Bool
    , files         :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { list_mcus = def &= help "list available MCUs by family"
    , build_rules = def &= help "generate source for build rules"
    , family_header = def &= help "generate family header (to directory)"
    , family = def &= help "filter on family"
    , sub_family = def &= help "filter on sub-family"
    , package = def &= help "filter on package"
    , parse_svd = def &= help "process svd files"
    , address_map = def &= help "show peripheral address map"
    , normal_svd = def &= help "normalize svd files"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate pin descriptions from STM32CubeMX xml files" &=
    summary "STM32Data v0.1.0, (c) Bengt Marten Agren 2020" &=
    details [ "STM32Data generate device header files for STM32"
            , "MCUs based on vendor XML files from SMT32CubeMX."
            ]

stm32CubeMX = "c:/Program Files (x86)/STMicroelectronics/STM32Cube/STM32CubeMX"
stm32DbDir = "db/mcu"
familiesXML = "families.xml"
svdDir = "c:/ST/STM32CubeIDE_1.3.0/STM32CubeIDE/plugins/com.st.stm32cube.ide.mcu.productdb.debug_1.3.0.202002181050/resources/cmsis/STMicroelectronics_CMSIS_SVD"

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    let dbDir = stm32CubeMX </> stm32DbDir
        fs = concat
            [ map (Family . T.pack) family
            , map (SubFamily . T.pack) sub_family
            , map (Package . T.pack) package
            ]
    families <- prune fs . parseFamilies <$> T.readFile (dbDir </> familiesXML)
    when list_mcus $ mcuList families
    when build_rules $ mapM_ (putStrLn . T.unpack) $ buildRules families
    whenJust family_header $ \dir -> forM_ families $ \(family, subFamilies) -> do
        let header = dir </> T.unpack (T.toLower family) <.> "h"
        putStrLn header
        mcus <- mapM (loadMCU dbDir) $ controllers subFamilies
        gss <- mapM (\x -> (x,) <$> gpioConfigSet dbDir x) =<< gpioConfigs dbDir family
        T.writeFile header $ T.unlines $ familyHeader family mcus gss
    when parse_svd $ forM_ families $ \(family, subFamilies) -> do
        let dir = svdDir
            pred x = T.unpack family `isPrefixOf` x && takeExtension x == ".svd"
            proc = if address_map then peripheralMap else prettySVD
        print family
        xs <- filter pred <$> getDirectoryContents dir
        ys <- forM xs $ \x -> do
            putStrLn $ "parsing " <> x
            parseSVD <$> T.readFile (dir </> x)
        if address_map then
            mapM_ T.putStrLn $ concatMap peripheralMap ys
        else if normal_svd then
            mapM_ print $ normalSVD ys
        else
            mapM_ T.putStrLn $ concatMap prettySVD ys

