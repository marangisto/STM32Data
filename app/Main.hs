{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Main where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace, toLower)
import Data.List (nub, sort, stripPrefix)
import Data.List.Extra (groupSort)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as H
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
import AltFun
import MCU as M

type Text = T.Text

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
        fs = map (Family . T.pack) family ++ map (SubFamily . T.pack) sub_family ++ map (Package . T.pack) package
    families <- prune fs . parseFamilies <$> T.readFile (dbDir </> familiesXML)
    when list_mcus $ mcuList families
    whenJust alt_fun $ \outputDir -> mapM_ (genAltFun dbDir outputDir) $ flatten families
    when build_rules $ mapM_ (putStrLn . T.unpack) $ buildRules families
    when normalize $ forM_ families $ \(family, subFamilies) -> do
        let outputDir = "c:/tmp/afnorm"
        xs <- forM [ c | (_, controllers) <- subFamilies, c <- controllers ] $ \controller -> do
            mcu@MCU{..} <- loadMCU dbDir $ name controller
            let ident = identFromRefName $ T.unpack $ F.refName controller
            putStrLn ident
            hFlush stdout
            return $! map (,components ident) $ alternateFunctions pins
        let ys = groupSort $ concat xs
        mapM_ print ys
        print $ length ys
        let zs = nub $ sort $ map (nub . sort . snd) ys
        mapM_ print zs
        print $ length zs
        forM_ (zip zs [0..]) $ \(xs, i) -> do
            putStrLn $ "group-" <> show i
            --forM_ xs $ \(a, b, c, d) -> putStrLn [ a, ' ', b, ' ', c, ' ', d ]
                
{-
            let hdr = T.unlines $ altFunDecl mcu
            return $! seq hdr (hdr, [controller])
        print $ length xs
        let h = H.fromListWith (++) xs
        print $ length $ H.keys h
        forM_ (H.toList h) $ \(hdr, cs@(controller:_)) -> do
            let uniqName = init $ T.unpack $ F.refName controller
            mcu <- loadMCU dbDir $ name controller
            let dir = outputDir </> map toLower (T.unpack family)
                outputFile = dir </> map toLower uniqName <.> "h"
            putStrLn $ uniqName <> " -> " <> outputFile
            hFlush stdout
            createDirectoryIfMissing True dir
            withFile outputFile WriteMode $ \h -> do
                hSetNewlineMode h noNewlineTranslation
                T.hPutStr h $ T.unlines $ concat
                    [ map F.refName cs
                    , preAmble controller
                    , altFunDecl mcu
                    ]
-}
{-
    when normalize $ forM_ families $ \(family, subFamilies) -> do
        let outputDir = "c:/tmp/afnorm"
        xs <- forM [ c | (_, controllers) <- subFamilies, c <- controllers ] $ \controller -> do
            mcu@MCU{..} <- loadMCU dbDir $ name controller
            putStrLn $ T.unpack refName
            hFlush stdout
            let hdr = T.unlines $ altFunDecl mcu
            return $! seq hdr (hdr, [controller])
        print $ length xs
        let h = H.fromListWith (++) xs
        print $ length $ H.keys h
        forM_ (H.toList h) $ \(hdr, cs@(controller:_)) -> do
            let uniqName = init $ T.unpack $ F.refName controller
            mcu <- loadMCU dbDir $ name controller
            let dir = outputDir </> map toLower (T.unpack family)
                outputFile = dir </> map toLower uniqName <.> "h"
            putStrLn $ uniqName <> " -> " <> outputFile
            hFlush stdout
            createDirectoryIfMissing True dir
            withFile outputFile WriteMode $ \h -> do
                hSetNewlineMode h noNewlineTranslation
                T.hPutStr h $ T.unlines $ concat
                    [ map F.refName cs
                    , preAmble controller
                    , altFunDecl mcu
                    ]
-}

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

genAltFun :: FilePath -> FilePath -> (Text, Text, Controller) -> IO ()
genAltFun dbDir outputDir (family, subFamily, controller) = do
    let uniqName = init $ T.unpack $ F.refName controller
    mcu <- loadMCU dbDir $ name controller
    let dir = outputDir </> map toLower (T.unpack family)
        outputFile = dir </> map toLower uniqName <.> "h"
    putStrLn $ uniqName <> " -> " <> outputFile
    hFlush stdout
    createDirectoryIfMissing True dir
    withFile outputFile WriteMode $ \h -> do
        hSetNewlineMode h noNewlineTranslation
        T.hPutStr h $ T.unlines $ concat
            [ preAmble controller
            , altFunDecl mcu
            ]

