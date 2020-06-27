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
import Families
import IPMode
import Pretty
import FrontEnd.ParseSVD
import NormalSVD
import Utils
import FrontEnd
import PrettyCPP

data Options = Options
    { list_mcus     :: Bool
    , build_rules   :: Bool
    , old_core      :: Bool
    , clock_control :: Bool
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
    , old_core = def &= help "run old core"
    , clock_control = def &= help "report missing and unused clock control"
    , family = def &= help "filter on family"
    , sub_family = def &= help "filter on sub-family"
    , package = def &= help "filter on package"
    , headers = def &= help "generate headers (to directory)"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate pin descriptions from STM32CubeMX xml files" &=
    summary "STM32Data v0.3.0, (c) Bengt Marten Agren 2020" &=
    details [ "STM32Data generate device header files for STM32"
            , "MCUs based on vendor XML files from SMT32CubeMX."
            ]

tmpDir :: FilePath
tmpDir = "C:" </> "tmp"

famDir = case os of
    "linux" -> "/usr/local/STMicroelectronics/STM32Cube"
    _       -> "C:/Program Files (x86)/STMicroelectronics/STM32Cube"

svdDir = case os of
    "linux" -> "/opt/st/stm32cubeide_1.3.0"
    _       -> "C:/ST/STM32CubeIDE_1.3.0"

main :: IO ()
main = do
    Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    let fs = concat
            [ map (OnFamily . T.pack) family
            , map (OnSubFamily . T.pack) sub_family
            , map (OnPackage . T.pack) package
            ]
    [famXML] <- cacheLines familiesFile famDir
    families' <- parseFamilies famXML
    families <- return $ prune fs families'
    allSVDs <- cacheLines svdFiles svdDir
    let dbDir = takeDirectory famXML

    unless old_core $
      forM_ families $ \(family', subFamilies) -> do
        fam@Family{..} <- processFamily svdDir dbDir family' subFamilies
        whenJust headers $ flip prettyCPP fam

    {-
        let svds = familySVDs family' allSVDs
        nsvd <- resolveCC . fixup . normalize family'
            <$> mapM parseSVD (map snd svds)

        when clock_control $ do
            T.putStrLn "========================================="
            T.putStrLn $ (\NormalSVD{..} -> family) nsvd
            T.putStrLn "-----------------------------------------"
            let putWords s = T.putStrLn . T.unwords . (T.pack s:)
            putWords "missing:" $ missingCC nsvd
            putWords "unused:" $ unusedCC nsvd

        mcuSpecs <- mapM (parseMCU $ map fst svds) $ mcuFiles dbDir subFamilies
        ipGPIOs <- mapM parseIpGPIO $ ipGPIOFiles dbDir mcuSpecs
        mapM_ (T.putStrLn . (\MCU{..} -> refName <> " " <> svd)) mcuSpecs
        mapM_ print ipGPIOs
        -}

    {-
        -- Mcu.name refers to MCU.refName
        let f Mcu{..} = (name, refName, rpn)
        mapM_ (print . f) $ controllers subFamilies
        let g MCU{..} = refName
        mapM_ (print . g) mcuSpecs
    -}

        -- matchSVD :: [Text] -> Text -> Maybe Text
            {-
        forM_ periphTypes $ \PeriphType{..} -> do
            print typeRef
            mapM_ (putStrLn . ("    "<>) . show) periphInsts
        forM_ interrupts $ \Interrupt{..} ->
            putStrLn $ unwords
                [ show value
                , T.unpack name
                , T.unpack description
                ]
                -}

    {-
        let ipGPIOs = nub $ sort
                [ version
                | MCU{..} <- mcuSpecs
                , IP{name="GPIO",..} <- ips
                ]
        ipGPIOs <- forM ipGPIOs $ \name ->
            parseIpGPIO
                ( dbDir
                </> "IP"
                </> "GPIO-" <> T.unpack name <> "_Modes"
                <.> "xml"
                )
        mapM_ print ipGPIOs
        -}

    when list_mcus $ mcuList families
    when build_rules
        $ mapM_ (putStrLn . T.unpack)
        $ buildRules families

    when old_core $ whenJust headers $ \top -> do
      stm32Header top $ map fst families'
      forM_ families $ \(family, subFamilies) -> do
        let dir = top </> T.unpack (T.toLower family) </> "device"
        createDirectoryIfMissing True dir
        let svds = familySVDs family allSVDs
        mcus <- mapM (loadMCU dbDir) $ controllers subFamilies
        gss <- mapM (\x -> (x,) <$> gpioConfigSet dbDir x)
            =<< gpioConfigs dbDir family
        let pfs = groupSort $ nub $ sort $ concatMap (periFuns . snd) gss
        familyHeaders dir family mcus gss $ map fst svds
        withTempDirectory tmpDir (T.unpack family) $ \tmp ->
          normalizeSVD tmp dir family pfs svds

stm32Header :: FilePath -> [Text] -> IO ()
stm32Header dir xs = do
    createDirectoryIfMissing False dir
    let header = dir </> "stm32" <.> "h"
    putStrLn $ "writing " <> header
    writeText header
        $ banner [ "STM32 MCU families" ]
        ++ enum "mcu_family_t" (map unPlus xs)

familySVDs :: Text -> [FilePath] -> [(Text, FilePath)]
familySVDs family = sort . filter pred . map f
    where f s = (T.pack $ dropExtension $ takeFileName s, s)
          pred (x, _)
            | fam == "STM32L4+" = isL4plus x
            | fam == "STM32G4" = any (`T.isPrefixOf` x) [ fam, "STM32GBK1" ]
            | otherwise = fam `T.isPrefixOf` x && not (isL4plus x)
            where fam = family

isL4plus :: Text -> Bool
isL4plus x = any (`T.isPrefixOf`x) $ map ("STM32L4"<>) [ "P", "Q", "R", "S" ]

svdHeader :: FilePath -> SVD -> FilePath
svdHeader dir SVD{..} = dir </> T.unpack (T.toLower name) <.> "h"

svdFiles :: FilePath -> IO [FilePath]
svdFiles = traverseDir (\_ -> True) accept []
    where accept :: [FilePath] -> FilePath -> IO [FilePath]
          accept xs fp
            | takeExtension fp == ".svd" = return $ fp : xs
            | otherwise = return xs

familiesFile :: FilePath -> IO [FilePath]
familiesFile = traverseDir (\_ -> True) accept []
    where accept :: [FilePath] -> FilePath -> IO [FilePath]
          accept xs fp
            | takeFileName fp == "families.xml" = return $ fp : xs
            | otherwise = return xs

