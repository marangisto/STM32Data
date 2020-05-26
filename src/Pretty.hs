{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
module Pretty
    ( familyHeaders
    ) where

import Numeric (showHex)
import Text.Read (readMaybe)
import Data.Char (ord)
import Data.List (nub, sort)
import Data.List.Extra (groupSort)
import qualified Data.Set as Set
import qualified Data.Text as T
import System.FilePath
import IPMode
import MCU
import Utils

familyHeaders
    :: FilePath
    -> Text
    -> [MCU]
    -> [(Text, Set.Set ((PIN, AF), Int))]
    -> [Text]
    -> IO ()
familyHeaders dir family mcus ys svds = do
    let h1 = dir </> "mcu" <.> "h"
    putStrLn h1
    writeText h1 $ mcuHeader family mcus ys svds
    let h2 = dir </> "pin" <.> "h"
    putStrLn h2
    writeText h2 $ pinHeader family svds mcus ys

mcuHeader
    :: Text
    -> [MCU]
    -> [(Text, Set.Set ((PIN, AF), Int))]
    -> [Text]
    -> [Text]
mcuHeader family mcus ys svds = concat $
    [ [ "#pragma once"
      , ""
      , "////"
      , "//"
      , "//      " <> family <> " MCUs"
      , "//"
      , "////"
      ]
    , enumDecl False "mcu_t" $ map name mcus
    , enumDecl False "mcu_svd_t" svds
    , enumDecl True "gpio_conf_t" $ map unGPIOConf confs
    , mcuTraitsDecl mcus svds
    , [ ""
      , "static constexpr mcu_t target = MCU;"
      ]
    ]
    where confs = map (cleanGPIOConf svds . fst) ys

pinHeader
    :: Text
    -> [Text]
    -> [MCU]
    -> [(Text, Set.Set ((PIN, AF), Int))]
    -> [Text]
pinHeader family svds mcus ys = concat $
    [ [ "#pragma once"
      , ""
      , "////"
      , "//"
      , "//      " <> family <> " pins"
      , "//"
      , "////"
      ]
    , enumDecl2 "gpio_port_t" [ (T.pack $ 'P' : p, i) | (p, i) <- ports ]
    , portTraitsDecl $ map (T.pack . fst) ports
    , enumDecl2 "gpio_pin_t" $ map (\(p, i) -> (T.pack $ 'P' : p : show i, (ord p - ord 'A') * 16 + i)) pins
    , funDecl $ maximum $ [ v | (_, xs) <- ss, (_, v) <- [ xs ] ]
    , afEnumDecl $ nub $ sort [ af | ((_, af), _) <- ss ]
    , [ ""
      , "static constexpr gpio_conf_t gpio_conf = mcu_traits<target>::gpio_conf;"
      ]
    , gpioTraitsDecl
    , map (gpioConfigTraitDecl confs) $ groupSort ss
    ]
    where confs = map (cleanGPIOConf svds . fst) ys
          ss = [ (p, (cleanGPIOConf svds conf, v))
               | (conf, s) <- ys
               , (p, v) <- Set.toList s
               ]
          ports = [ ([ p ], ord p - ord 'A')
                  | p <- nub . sort $ map fst pins
                  ]
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

cleanGPIOConf :: [Text] -> Text -> GPIOConf
cleanGPIOConf svds s
    | clean `elem` svds = GPIOConf $ clean <> "_" -- avoid conflict
    | otherwise = GPIOConf clean
    where clean = fst $ T.breakOn "_" s

enumDecl :: Bool -> Text -> [Text] -> [Text]
enumDecl bits name xs = concat
    [ [ "", "enum " <> name ]
    , [ s <> x <> if bits then " = 0x" <> T.pack (showHex ((2::Int)^i) "") else ""
      | (s, x, i) <- zip3 ("    { " : repeat "    , ") (sort xs) [(0::Int)..]
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

portTraitsDecl :: [Text] -> [Text]
portTraitsDecl ports =
    [ ""
    , "template<gpio_port_t PORT> struct gpio_traits_t {};"
    ] ++ concatMap f ports
    where f port =
            [ ""
            , "template<> struct gpio_traits_t<P" <> port <> ">"
            , "{"
            , "    static constexpr peripheral_enum_t "
            <> "peripheral = GPIO" <> port <> ";"
            , "};"
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
value (v, _) Nothing = "AF" <> T.pack (show v)
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

mcuTraitsDecl :: [MCU] -> [Text] -> [Text]
mcuTraitsDecl mcus svds =
    [ ""
    , "template<mcu_t MCU> struct mcu_traits {};"
    ] ++ concatMap f mcus
    where f MCU{..} =
            [ ""
            , "template<> struct mcu_traits<" <> name <> ">"
            , "{"
            , "    static constexpr mcu_family_t mcu_family = " <> unPlus family <> ";"
            ] ++
            [ "    static constexpr mcu_svd_t mcu_svd = " <> svd <> ";"
            | Just svd <- [ matchSVD svds name ]
            ] ++
            [ "    static constexpr gpio_conf_t gpio_conf = " <> gc <> ";"
            , "};"
            ]
            where gc = unGPIOConf $ cleanGPIOConf svds gpioConfig

matchSVD :: [Text] -> Text -> Maybe Text
matchSVD svds name = case filter p svds of
    [] | "STM32F105" `T.isPrefixOf` name -> Just "STM32F107"
    [] | "STM32F205" `T.isPrefixOf` name -> Just "STM32F215"
    [] | "STM32F207" `T.isPrefixOf` name -> Just "STM32F217"
    [] | "STM32F415" `T.isPrefixOf` name -> Just "STM32F405"
    [] | "STM32F417" `T.isPrefixOf` name -> Just "STM32F407"
    [] | "STM32F423" `T.isPrefixOf` name -> Just "STM32F413"
    [] | "STM32F437" `T.isPrefixOf` name -> Just "STM32F427"
    [] | "STM32F439" `T.isPrefixOf` name -> Just "STM32F429"
    [] | "STM32F479" `T.isPrefixOf` name -> Just "STM32F469"
    [] | "STM32GBK1" `T.isPrefixOf` name -> Nothing
    [] | "STM32H745BG" `T.isPrefixOf` name -> Nothing
    [] ->  error $ "failed to match svd for " <> T.unpack name
    [ svd ] -> Just svd
    xs -> case filter tame xs of
        [ svd ] -> Just svd
        _ -> error $ T.unpack name <> " matches " <> show xs
    where p = all match . zip (T.unpack name) . T.unpack . chooseM4
          match (n, s) = n == s || s == 'x'
          chooseM4 svd | Just x <- T.stripSuffix "_CM4" svd = x
                       | otherwise = svd
          tame = not . any (=='x') . T.unpack

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

