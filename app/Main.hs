{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections, DuplicateRecordFields, OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs hiding (name)
import System.FilePath
import System.Directory
import System.IO
import System.IO.Temp
import Control.Monad
import Control.Monad.Extra
import Data.List (sort, isPrefixOf)
import Data.Hashable
import Family as F
import IPMode
import Pretty
import ParseSVD as P
import PrettySVD
import NormalSVD as N

type Text = T.Text

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
    , normal_svd    :: Bool
    , normalize     :: Bool
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
    , normal_svd = def &= help "normalize svd files"
    , normalize = def &= help "normalize svd files"
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
    whenJust family_header $ \dir -> forM_ families $ \(family, subFamilies) -> do
        let header = dir </> T.unpack (T.toLower family) <.> "h"
        putStrLn header
        mcus <- mapM (loadMCU dbDir) $ controllers subFamilies
        gss <- mapM (\x -> (x,) <$> gpioConfigSet dbDir x) =<< gpioConfigs dbDir family
        T.writeFile header $ T.unlines $ familyHeader family mcus gss

    whenJust svd_header $ \dir -> forM_ families $ \(family, subFamilies) -> do
        T.putStrLn family
        xs <- svdFiles family
        forM_ xs $ \(x, fn) -> do
            putStrLn $ "parsing " <> fn
            svd <- parseSVD <$> T.readFile fn
            let header = svdHeader dir svd
            putStrLn $ "writing " <> header
            T.writeFile header $ T.unlines $ prettySVD svd

    when normal_svd $ forM_ families $ \(family, subFamilies) -> do
        T.putStrLn family
        xs <- svdFiles family
        ys <- forM xs $ \(x, fn) -> do
            putStrLn $ "parsing " <> fn
            parseSVD <$> T.readFile fn
        when address_map $ mapM_ T.putStrLn $ concatMap peripheralMap ys
        mapM_ print $ normalSVD ys

    when normalize $ forM_ families $ \(family, subFamilies) -> withTempDirectory tmpDir (T.unpack family) $ \tmp -> do
        putStrLn $ T.unpack family <> " in " <> tmp
        xs <- svdFiles family
        ys <- forM xs $ \(x, fn) -> do
            putStrLn $ "parsing " <> fn
            svd <- parseSVD <$> T.readFile fn
            processSVD tmp svd
        mapM_ print $ concat ys
        let ps = [ text | Representative{..} <- concat ys ]
        putStrLn $ "number of peripherals = " <> show (length ps)

data Normalization
    = Representative
    { svdName   :: Text
    , name      :: Text
    , digest    :: Int
    , text      :: FilePath
    }
    | Normalization
    { svdName   :: Text
    , name      :: Text
    , digest    :: Int
    } deriving (Show)

processSVD :: FilePath -> SVD -> IO [Normalization]
processSVD tmp SVD{..} = do
    putStrLn $ "processing " <> T.unpack name
    mapM (processPeripheral tmp name)
        [ p
        | p@Peripheral{..} <- peripherals
        , Nothing <- [ derivedFrom ]
        ]

processPeripheral :: FilePath -> Text -> P.Peripheral -> IO Normalization
processPeripheral tmp svdName p@Peripheral{..} = do
    let h = hash p
        fn = tmp </> show (abs h) <.> "h"
    already <- doesFileExist fn
    if already then do
        putStrLn $ T.unpack name <> " normalized"
        return Normalization{digest=h,..}
    else do
        putStr $ T.unpack name <> fn <> "..."
        T.writeFile fn $ T.unlines $ prettyPeripheral p
        putStrLn $ "done"
        return Representative{digest=h,text=fn,..}

svdFiles :: Text -> IO [(Text, FilePath)]
svdFiles family = do
    xs <- sort . filter pred <$> getDirectoryContents dir
    return $ map (\s -> (T.pack $ dropExtension s, dir </> s)) xs
    where pred x = T.unpack family `isPrefixOf` x && takeExtension x == ".svd"
          dir = case T.unpack family of
              "STM32MP1" -> svdDir'
              _ -> svdDir

svdHeader :: FilePath -> SVD -> FilePath
svdHeader dir SVD{..} = dir </> T.unpack (T.toLower name) <.> "h"

