{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
module Pretty
    ( familyHeader
    ) where

import Numeric (showHex)
import Text.Read (readMaybe)
import Data.Char (ord)
import Data.List (nub, sort)
import Data.List.Extra (groupSort)
import qualified Data.Set as Set
import qualified Data.Text as T
import IPMode
import MCU

type Text = T.Text

familyHeader :: Text -> [MCU] -> [(Text, Set.Set ((PIN, AF), Int))] -> [Text]
familyHeader family mcus ys = concat $
    [ [ "#pragma once"
      , ""
      , "////"
      , "//"
      , "//      " <> family
      , "//"
      , "////"
      ]
    , enumDecl False "mcu_t" $ map name mcus
    , enumDecl True "gpio_conf_t" $ map unGPIOConf confs
    , mcuTraitsDecl mcus
    , [ ""
      , "static constexpr mcu_t target = MCU;"
      , "static constexpr gpio_conf_t gpio_conf = mcu_traits<target>::gpio_conf;"
      ]
    , enumDecl2 "gpio_port_t" $ map (\p -> (T.pack [ 'P', p ], ord p - ord 'A')) ports
    , enumDecl2 "gpio_pin_t" $ map (\(p, i) -> (T.pack $ 'P' : p : show i, (ord p - ord 'A') * 16 + i)) pins
    , funDecl $ maximum $ [ v | (_, xs) <- ss, (_, v) <- [ xs ] ]
    , afEnumDecl $ nub $ sort [ af | ((_, af), _) <- ss ]
    , gpioTraitsDecl
    , map (gpioConfigTraitDecl confs) $ groupSort ss
    ]
    where confs = map (cleanGPIOConf . fst) ys
          ss = [ (p, (cleanGPIOConf conf, v))
               | (conf, s) <- ys
               , (p, v) <- Set.toList s
               ]
          ports = nub . sort $ map fst pins
          pins = ioPins mcus

ioPins :: [MCU] -> [(Char, Int)]
ioPins mcus = nub $ sort
    [ portPin
    | MCU{..} <- mcus
    , IOPin{..} <- pins
    , Just portPin <- [ splitPin $ T.unpack pinName ]
    ]

splitPin :: String -> Maybe (Char, Int)
splitPin ('P':p:xs) = (p,) <$> readMaybe xs
splitPin _ = Nothing

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

enumDecl2 :: Text -> [(Text, Int)] -> [Text]
enumDecl2 name xs = concat
    [ [ "", "enum " <> name ]
    , [ s <> x <> " = 0x" <> T.pack (showHex i "")
      | (s, (x, i)) <- zip ("    { " : repeat "    , ") xs
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
constraint [x] = "gpio_conf & " <> unGPIOConf x
constraint xs = "gpio_conf & (" <> T.intercalate "|" (map unGPIOConf xs) <> ")"

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

mcuTraitsDecl :: [MCU] -> [Text]
mcuTraitsDecl mcus =
    [ ""
    , "template<mcu_t MCU> struct mcu_traits {};"
    ] ++ concatMap f mcus
    where f MCU{..} =
            [ ""
            , "template<> struct mcu_traits<" <> name <> ">"
            , "{"
            , "    static constexpr gpio_conf_t gpio_conf = " <> gc <> ";"
            , "};"
            ]
            where gc = unGPIOConf $ cleanGPIOConf gpioConfig

gpioTraitsDecl :: [Text]
gpioTraitsDecl =
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
