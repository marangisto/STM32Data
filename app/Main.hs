{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections, DuplicateRecordFields, OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs hiding (name, enum)
import System.FilePath
import System.Directory
import System.IO
import System.IO.Temp
import Control.Monad
import Control.Monad.Extra
import Data.List (nub, sort, isPrefixOf)
import Data.List.Extra (groupSort)
import Family
import ParseMCU
import ParseIpGPIO
import IPMode
import Pretty
import ParseSVD
import NormalSVD
import Utils

data Options = Options
    { list_mcus     :: Bool
    , build_rules   :: Bool
    , new_core      :: Bool
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
    , new_core = def &= help "run new core"
    , family = def &= help "filter on family"
    , sub_family = def &= help "filter on sub-family"
    , package = def &= help "filter on package"
    , headers = def &= help "generate headers (to directory)"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate pin descriptions from STM32CubeMX xml files" &=
    summary "STM32Data v0.1.0, (c) Bengt Marten Agren 2020" &=
    details [ "STM32Data generate device header files for STM32"
            , "MCUs based on vendor XML files from SMT32CubeMX."
            ]

stm32CubeMX :: FilePath
stm32CubeMX = "C:/Program Files (x86)/STMicroelectronics/STM32Cube/STM32CubeMX"

stm32DbDir :: FilePath
stm32DbDir = "db/mcu"

familiesXML :: FilePath
familiesXML = "families.xml"

svdDir, svdDir' :: FilePath
svdDir = "C:/ST/STM32CubeIDE_1.3.0/STM32CubeIDE/plugins/com.st.stm32cube.ide.mcu.productdb.debug_1.3.0.202002181050/resources/cmsis/STMicroelectronics_CMSIS_SVD"
svdDir' = "C:/ST/STM32CubeIDE_1.3.0/STM32CubeIDE/plugins/com.st.stm32cube.ide.mpu.productdb.debug_1.3.0.202002181049/resources/cmsis/STMicroelectronics_CMSIS_SVD"

tmpDir :: FilePath
tmpDir = "C:/tmp"

main :: IO ()
main = do
    Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    let dbDir = stm32CubeMX </> stm32DbDir
        fs = concat
            [ map (Family . T.pack) family
            , map (SubFamily . T.pack) sub_family
            , map (Package . T.pack) package
            ]

    families' <- parseFamilies (dbDir </> familiesXML)
    families <- return $ prune fs families'

    when new_core $
      forM_ families $ \(family, subFamilies) -> do
        let mcuSpecs = nub $ sort
                [ name
                | Controller{..} <- controllers subFamilies
                ]
        mcuSpecs <- forM mcuSpecs $ \name ->
            parseMCU (dbDir </> T.unpack name <.> "xml")
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

    when list_mcus $ mcuList families
    when build_rules
        $ mapM_ (putStrLn . T.unpack)
        $ buildRules families

    whenJust headers $ \top -> do
      stm32Header top $ map fst families'
      forM_ families $ \(family, subFamilies) -> do
        let dir = top </> T.unpack (T.toLower family) </> "device"
        createDirectoryIfMissing True dir
        svds <- svdFiles family
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

svdFiles :: Text -> IO [(Text, FilePath)]
svdFiles family = do
    xs <- sort . filter pred . filter isSVD <$> getDirectoryContents dir
    return $ map (\s -> (T.pack $ dropExtension s, dir </> s)) xs
    where isSVD x = takeExtension x == ".svd"
          pred x
            | fam == "STM32L4+" = isL4plus x
            | otherwise = fam `isPrefixOf` x && not (isL4plus x)
            where fam = T.unpack family
          dir = case T.unpack family of
              "STM32MP1" -> svdDir'
              _ -> svdDir

isL4plus :: String -> Bool
isL4plus x = any (`isPrefixOf`x) $ map ("STM32L4"<>) [ "P", "Q", "R", "S" ]

svdHeader :: FilePath -> SVD -> FilePath
svdHeader dir SVD{..} = dir </> T.unpack (T.toLower name) <.> "h"

