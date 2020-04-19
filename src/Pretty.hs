{-# LANGUAGE OverloadedStrings #-}
module Pretty
    ( familyHeader
    , gpioConfigHeader
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

familyHeader :: [Text] -> [Text]
familyHeader xs = concat $
    [ [ "#pragma once"
      , ""
      , "#include <type_traits>"
      ]
    , enumDecl False "mcu_t" xs
    ]

gpioConfigHeader :: [(Text, Set.Set ((PIN, AF), Int))] -> [Text]
gpioConfigHeader ys = concat $
    [ enumDecl True "gpio_conf_t" $ map unGPIOConf confs
    , [ "", "static constexpr gpio_conf_t GPIOConf = " <> unGPIOConf (head confs) <> ";" ]
    , funDecl $ maximum $ [ v | (_, xs) <- ss, (_, v) <- [ xs ] ]
    , afEnumDecl $ nub $ sort [ af | ((_, af), _) <- ss ]
    , traitsDecl
    , map (gpioConfigTraitDecl confs) $ groupSort ss
    ]
    where confs = map (cleanGPIOConf . fst) ys
          ss = [ (p, (cleanGPIOConf conf, v))
               | (conf, s) <- ys
               , (p, v) <- Set.toList s
               ]

cleanGPIOConf :: Text -> GPIOConf
cleanGPIOConf = GPIOConf . fst . T.breakOn "_"

enumDecl :: Bool -> Text -> [Text] -> [Text]
enumDecl bits name xs = concat
    [ [ "", "enum " <> name ]
    , [ s <> x <> if bits then " = 0x" <> T.pack (showHex (2^i) "") else ""
      | (s, x, i) <- zip3 ("    { " : repeat "    , ") (sort xs) [0..]
      ]
    , [ "    };" ]
    ]

gpioConfigTraitDecl :: [GPIOConf] -> ((PIN, AF), [(GPIOConf, Int)]) -> Text
gpioConfigTraitDecl confs ((pin, altfun), confVals) = T.concat
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
    where t | all (`elem` map fst confVals) confs = "alt_fun_t"
            | otherwise = "alt_fun<" <> constraint ms <> ">"
          v | [ p ] <- valConfs = value p Nothing
            | [ p, q ] <- valConfs = value p $ Just q
            | otherwise = error "too complex"
          valConfs = groupSort [ (v, m) | (m, v) <- confVals ]
          ms = map fst confVals

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

