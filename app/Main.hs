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
import Data.List (sort, isPrefixOf)
import Family
import IPMode
import Pretty
import ParseSVD
import NormalSVD
import Utils

data Options = Options
    { list_mcus     :: Bool
    , build_rules   :: Bool
    , family_header :: Maybe FilePath
    , family        :: [String]
    , sub_family    :: [String]
    , package       :: [String]
    , svd_header    :: Maybe FilePath
    , parse_svd     :: Bool
    , address_map   :: Bool
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
    , svd_header = def &= help "generate svd header (to directory)"
    , parse_svd = def &= help "process svd files"
    , address_map = def &= help "show peripheral address map"
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
svdDir' = "c:/ST/STM32CubeIDE_1.3.0/STM32CubeIDE/plugins/com.st.stm32cube.ide.mpu.productdb.debug_1.3.0.202002181049/resources/cmsis/STMicroelectronics_CMSIS_SVD"
tmpDir = "c:/tmp"

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

    whenJust svd_header $ \dir -> do
      stm32Header dir $ map fst families
      forM_ families $ \(family, _) ->
        withTempDirectory tmpDir (T.unpack family) $ \tmp ->
          normalizeSVD tmp dir family =<< svdFiles family

    whenJust family_header $ \dir ->
      forM_ families $ \(family, subFamilies) -> do
        mcus <- mapM (loadMCU dbDir) $ controllers subFamilies
        gss <- mapM (\x -> (x,) <$> gpioConfigSet dbDir x)
            =<< gpioConfigs dbDir family
        svds <- map fst <$> svdFiles family
        familyHeaders dir family mcus gss svds

stm32Header :: FilePath -> [Text] -> IO ()
stm32Header dir xs = do
    createDirectoryIfMissing False dir
    let header = dir </> "stm32" <.> "h"
    putStrLn $ "writing " <> header
    T.writeFile header $ T.unlines
        $ banner [ "STM32 MCU families" ]
        ++ enum "mcu_families_t" xs

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

