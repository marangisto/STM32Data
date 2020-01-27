{-# LANGUAGE RecordWildCards #-}
module AltFun (pretty) where

import System.FilePath
import Control.Arrow
import Data.List (nub, sort)
import Family as F
import IPMode
import MCU as M

pretty :: FilePath -> FilePath -> F.MCU -> IO ()
pretty dbDir outputDir fmcu = do
    let uniqName = init $ F.refName fmcu
    putStrLn $ uniqName
    mcu@M.MCU{..} <- parseMCU <$> readFile (dbDir </> name fmcu <.> "xml")
    afmap <- altFunMap <$> readFile (dbDir </> "IP" </> "GPIO-" <> gpioConfig <> "_Modes" <.> "xml")
    mcu@M.MCU{..} <- return $ mcu { pins = map (resolveFunctions afmap) pins }
    let afs = alternateFunctions pins
    mapM_ putStrLn $ concat
        [ preAmble uniqName fmcu
        , funDecl $ maximum $ map (\(_, _, i) -> i) afs
        , [ "" ]
        , enumDecl . nub . sort $ map (\(_, s, _) -> s) afs
        , [ "" ]
        , traitDecl
        , [ "" ]
        , map traitSpec afs
        , [ "" ]
        ]

preAmble :: String -> F.MCU -> [String]
preAmble uniqName F.MCU{..} =
    [ "#pragma once"
    , ""
    , "###"
    , "#"
    , "#        " <> uniqName
    , "#"
    ] ++ map fmt
    [ ("core", core)
    , ("package", package)
    , ("frequency", show frequency)
    , ("flash", show flash <> "kB")
    , ("ram", show ram <> "kB")
    , ("IO count", show numIO)
    ] ++ map (fmt . second show) peripherals ++
    [ "#"
    , "###"
    , ""
    ]
    where fmt :: (String, String) -> String
          fmt (l, s) = "#        "<> l <> replicate (12 - length l) ' ' <> ": " <> s

funDecl :: Int -> [String]
funDecl n = concat
    [ [ "enum alt_fun_t" ]
    , [ s <> "AF" <> show i | (s, i) <- zip ("    { " : repeat "    , ") [0..n-1] ]
    , [ "    };" ]
    ]

enumDecl :: [String] -> [String]
enumDecl xs = concat
    [ [ "enum alternate_function_t" ]
    , [ s <> x | (s, x) <- zip ("    { " : repeat "    , ") xs ]
    , [ "    };" ]
    ]

traitDecl :: [String]
traitDecl =
    [ "template<gpio_pin_t PIN, alternate_function_t ALT>"
    , "struct alt_fun_traits"
    , "{"
    , "    static_assert(always_false_i<PIN>::value, \"selected alternate function is not available on this pin!\");"
    , "};"
    ]

traitSpec :: (String, String, Int) -> String
traitSpec (p, s, i) = mconcat
    [ "template<> struct alt_fun_traits<"
    , p
    , ", "
    , s
    , "> { static const alt_fun_t AF = AF"
    , show i
    , "; };"
    ]
