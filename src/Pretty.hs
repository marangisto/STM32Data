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
import AltFun

type Text = T.Text

familyHeader :: [(Text, Set.Set ((PIN, AF), Int))] -> [Text]
familyHeader ys = concat $
    [ [ "#pragma once" ]
    , mcuEnumDecl mcus
    , funDecl $ maximum $ [ v | (_, xs) <- ss, (_, v) <- [ xs ] ]
    , afEnumDecl $ nub $ sort [ af | ((_, af), _) <- ss ]
    , map (gpioConfigTraitDecl mcus) $ groupSort ss
    ]
    where mcus = map (cleanMCU . fst) ys
          ss = [ (p, (cleanMCU conf, v))
               | (conf, s) <- ys
               , (p, v) <- Set.toList s
               ]

cleanMCU :: Text -> MC
cleanMCU = MC . fst . T.breakOn "_"

mcuEnumDecl :: [MC] -> [Text]
mcuEnumDecl xs = concat
    [ [ "", "enum gpio_config_t" ]
    , [ s <> unMC x <> " = 0x" <> T.pack (showHex (2^i) "")
      | (s, x, i) <- zip3 ("    { " : repeat "    , ") (sort xs) [0..]
      ]
    , [ "    };" ]
    ]

gpioConfigTraitDecl :: [MC] -> ((PIN, AF), [(MC, Int)]) -> Text
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

value :: (Int, [MC]) -> Maybe (Int, [MC]) -> Text
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

constraint :: [MC] -> Text
constraint [] = "true"
constraint [x] = "MCU & " <> unMC x
constraint xs = "MCU & (" <> T.intercalate "|" (map unMC xs) <> ")"

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

