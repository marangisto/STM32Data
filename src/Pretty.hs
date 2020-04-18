{-# LANGUAGE OverloadedStrings #-}
module Pretty
    ( familyHeader
    ) where

import Numeric (showHex)
import Data.Char (toLower)
import Data.List (stripPrefix, break, nub, sort)
import Data.List.Extra (groupSort)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO
import Family as F
import IPMode

type Text = T.Text

familyHeader :: [(Text, Set.Set ((PIN, AF), Int))] -> [Text]
familyHeader ys = concat $
    [ [ "#pragma once"
      , ""
      , "#include <type_traits>"
      ]
    , mcuEnumDecl mcus
    , [ "", "static constexpr gpio_conf_t GPIOConf = " <> unGPIOConf (head mcus) <> ";" ]
    , funDecl $ maximum $ [ v | (_, xs) <- ss, (_, v) <- [ xs ] ]
    , afEnumDecl $ nub $ sort [ af | ((_, af), _) <- ss ]
    , traitsDecl
    , map (gpioConfigTraitDecl mcus) $ groupSort ss
    ]
    where mcus = map (cleanGPIOConf . fst) ys
          ss = [ (p, (cleanGPIOConf conf, v))
               | (conf, s) <- ys
               , (p, v) <- Set.toList s
               ]

cleanGPIOConf :: Text -> GPIOConf
cleanGPIOConf = GPIOConf . fst . T.breakOn "_"

mcuEnumDecl :: [GPIOConf] -> [Text]
mcuEnumDecl xs = concat
    [ [ "", "enum gpio_conf_t" ]
    , [ s <> unGPIOConf x <> " = 0x" <> T.pack (showHex (2^i) "")
      | (s, x, i) <- zip3 ("    { " : repeat "    , ") (sort xs) [0..]
      ]
    , [ "    };" ]
    ]

gpioConfigTraitDecl :: [GPIOConf] -> ((PIN, AF), [(GPIOConf, Int)]) -> Text
gpioConfigTraitDecl mcus ((pin, altfun), mcuVals) = T.concat
    [ ""
    , "template<> struct alt_fun_traits<"
    , unPIN pin
    , ", "
    , unAF altfun
    , "> { static const "
    , t
    , " AF = "
    , v
    , "; };"
    ]
    where t | all (`elem` map fst mcuVals) mcus = "alt_fun_t"
            | otherwise = "alt_fun<" <> constraint ms <> ">"
          v | [ p ] <- valMcus = value p Nothing
            | [ p, q ] <- valMcus = value p $ Just q
            | otherwise = error "too complex"
          valMcus = groupSort [ (v, m) | (m, v) <- mcuVals ]
          ms = map fst mcuVals

value :: (Int, [GPIOConf]) -> Maybe (Int, [GPIOConf]) -> Text
value p@(v, _) Nothing = "AF" <> T.pack (show v)
value p@(v, xs) (Just q@(u, ys))
    | length xs > length ys = value q $ Just p
    | otherwise = T.concat
        [ "("
        , constraint xs
        , ") ? AF"
        , T.pack (show v)
        , " : AF"
        , T.pack (show u)
        ]

constraint :: [GPIOConf] -> Text
constraint [] = "true"
constraint [x] = "GPIOConf & " <> unGPIOConf x
constraint xs = "GPIOConf & (" <> T.intercalate "|" (map unGPIOConf xs) <> ")"

funDecl :: Int -> [Text]
funDecl n = concat
    [ [ "", "enum alt_fun_t" ]
    , [ s <> "AF" <> T.pack (show i) | (s, i) <- zip ("    { " : repeat "    , ") [0..n] ]
    , [ "    };" ]
    ]

afEnumDecl :: [AF] -> [Text]
afEnumDecl xs = concat
    [ [ "", "enum alternate_function_t" ]
    , [ s <> unAF x | (s, x) <- zip ("    { " : repeat "    , ") xs ]
    , [ "    };" ]
    ]

traitsDecl :: [Text]
traitsDecl =
   [ ""
   , "template<gpio_pin_t PIN, alternate_function_t ALT>"
   , "struct alt_fun_traits"
   , "{"
   , "    static constexpr alt_fun_t AF = AF0;"
   , ""
   , "    static_assert"
   , "        ( always_false_i<PIN>::value"
   , "        , \"alternate function not available on pin!\""
   , "        );"
   , "};"
   , ""
   , "template<bool AVAIL>"
   , "struct available_alt_fun_t"
   , "{"
   , "    typedef alt_fun_t type;"
   , ""
   , "    static_assert"
   , "        ( always_false_i<AVAIL>::value"
   , "        , \"alternate function not available on pin for target mcu!\""
   , "        );"
   , "};"
   , ""
   , "template<>"
   , "struct available_alt_fun_t<true>"
   , "{"
   , "    typedef alt_fun_t type;"
   , "};"
   , ""
   , "template<bool AVAIL>"
   , "using alt_fun = typename available_alt_fun_t<AVAIL>::type;"
   ]

