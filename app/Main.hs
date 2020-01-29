{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Main where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace, toLower)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Console.CmdArgs hiding (name)
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Control.Monad.Extra
import Family
import IPMode
import AltFun

data Options = Options
    { list_mcus     :: Bool
    , alt_fun       :: Maybe FilePath
    , build_rules   :: Bool
    , normalize     :: Bool
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
    , normalize = def &= help "normalize pins to determine minimal headers"
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
    whenJust alt_fun $ \outputDir -> mapM_ (genAltFun dbDir outputDir)
        [ (family, controller)
        | (family, subFamilies) <- families
        , (subFamily, controllers) <- subFamilies
        , controller <- controllers
        ]
    when build_rules $ mapM_ putStrLn $ buildRules families
    when normalize $ do
        decls <- mapM (\name -> altFunDecl <$> loadMCU dbDir name)
            [ name controller
            | (family, subFamilies) <- families
            , (subFamily, controllers) <- subFamilies
            , controller <- controllers
            ]
        print $ length $ nub $ sort $ decls

genAltFun :: FilePath -> FilePath -> (String, Controller) -> IO ()
genAltFun dbDir outputDir (family, controller) = do
    let uniqName = init $ refName controller
    mcu <- loadMCU dbDir $ name controller
    let dir = outputDir </> map toLower family
        outputFile = dir </> map toLower uniqName <.> "h"
    putStrLn $ uniqName <> " -> " <> outputFile
    hFlush stdout
    createDirectoryIfMissing True dir
    withFile outputFile WriteMode $ \h -> do
        hSetNewlineMode h noNewlineTranslation
        hPutStr h $ unlines $ concat
            [ preAmble controller
            , altFunDecl mcu
            ]

