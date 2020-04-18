{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Pretty
    ( familyHeader
    ) where

import Data.Char (toLower)
import Data.List (stripPrefix, break)
import Data.List.Extra (groupSort)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO
import Family as F
import IPMode
import AltFun

type Text = T.Text

familyHeader ys = do
        let s1 = foldl1 Set.union $ map snd ys
            s2 = Set.fromList $ map fst $ Set.toList s1

        mapM_ putStrLn $ gpioConfigEnumDecl $ map (cleanGpioConfig . fst) ys

        let ss = [ (p, (cleanGpioConfig conf, v)) | (conf, s) <- ys, (p, v) <- Set.toList s ]

        mapM_ (putStrLn . unlines . gpioConfigTraitDecl) $ groupSort ss


cleanGpioConfig :: Text -> Text
cleanGpioConfig = T.pack . fst . break (=='_') . T.unpack

gpioConfigEnumDecl :: [Text] -> [String]
gpioConfigEnumDecl xs = concat
    [ [ "enum gpio_config_t" ]
    , [ s <> T.unpack x <> " = (1 << " <> show i <> ")"
      | (s, x, i) <- zip3 ("    { " : repeat "    , ") xs [0..]
      ]
    , [ "    };" ]
    ]

gpioConfigTraitDecl :: ((Text, Text), [(Text, Int)]) -> [String]
gpioConfigTraitDecl ((pin, altfun), ((conf, val):[])) =
    [ "template<> struct alt_fun_traits<" <> T.unpack pin <> ", " <> T.unpack altfun <> ">"
    , "{"
    , "    static const alt_fun_t AF = AF" <> show val <> ";"
    , "};"
    ]
gpioConfigTraitDecl ((pin, altfun), xs) = []

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

