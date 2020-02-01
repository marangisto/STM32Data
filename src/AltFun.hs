{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module AltFun (altFunDecl) where

import System.IO
import System.FilePath
import System.Directory
import Control.Arrow
import Data.List (nub, sort)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TagSoup
import Family as F
import IPMode
import MCU

genAltFun :: FilePath -> FilePath -> Controller -> IO ()
genAltFun dbDir outputDir controller = do
    let uniqName = init $ T.unpack $ F.refName controller
    mcu <- loadMCU dbDir $ name controller
    let dir = outputDir </> map toLower (T.unpack $ family mcu)
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

altFunDecl :: MCU -> [Text]
altFunDecl MCU{..} = concat
    [ funDecl $ maximum $ map (\(_, _, i) -> i) afs
    , [ "" ]
    , enumDecl . nub . sort $ map (\(_, s, _) -> s) afs
    , [ "" ]
    , traitDecl
    , [ "" ]
    , map traitSpec afs
    , [ "" ]
    ]
    where afs = alternateFunctions pins

funDecl :: Int -> [Text]
funDecl n = concat
    [ [ "enum alt_fun_t" ]
    , [ s <> "AF" <> T.pack (show i) | (s, i) <- zip ("    { " : repeat "    , ") [0..n] ]
    , [ "    };" ]
    ]

enumDecl :: [Text] -> [Text]
enumDecl xs = concat
    [ [ "enum alternate_function_t" ]
    , [ s <> x | (s, x) <- zip ("    { " : repeat "    , ") xs ]
    , [ "    };" ]
    ]

traitDecl :: [Text]
traitDecl =
    [ "template<gpio_pin_t PIN, alternate_function_t ALT>"
    , "struct alt_fun_traits"
    , "{"
    , "    static_assert(always_false_i<PIN>::value, \"selected alternate function is not available on this pin!\");"
    , "};"
    ]

traitSpec :: (Text, Text, Int) -> Text
traitSpec (p, s, i) = mconcat
    [ "template<> struct alt_fun_traits<"
    , p
    , ", "
    , s
    , "> { static const alt_fun_t AF = AF"
    , T.pack $ show i
    , "; };"
    ]

