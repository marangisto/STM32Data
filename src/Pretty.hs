{-# LANGUAGE OverloadedStrings #-}
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

familyHeader :: [(Text, Set.Set ((PIN, AF), Int))] -> [Text]
familyHeader ys = concat $
    [ [ "#pragma once" ]
    , mcuEnumDecl mcus
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
    [ [ "enum gpio_config_t" ]
    , [ s <> unMC x <> " = (1 << " <> T.pack (show i) <> ")"
      | (s, x, i) <- zip3 ("    { " : repeat "    , ") xs [0..]
      ]
    , [ "    };" ]
    ]

gpioConfigTraitDecl :: [MC] -> ((PIN, AF), [(MC, Int)]) -> Text
gpioConfigTraitDecl mcus ((pin, altfun), mcuVals) = T.concat
    [ "template<> struct alt_fun_traits<"
    , unPIN pin
    , ", "
    , unAF altfun
    , "> { static const alt_fun<"
    , c
    , "> AF = "
    , v
    , "; };"
    ]
    where c | all (`elem` map fst mcuVals) mcus = constraint []
            | otherwise = constraint ms
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

