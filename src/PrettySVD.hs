{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module PrettySVD (prettySVD, prettyPeripheral, peripheralMap) where

import qualified Data.Text as T
import Data.List (sortOn, partition)
import Data.Bits (shift)
import Data.Maybe (isJust)
import Data.Char (isAscii)
import ParseSVD
import Utils

prettySVD :: SVD -> [Text]
prettySVD SVD{..} =
    [ "#pragma once"
    ] ++
    banner [ name, "Version " <> version ] ++
    [ ""
    , "#include <stdint.h>"
    , ""
    , "template<int N> class reserved_t { private: uint32_t m_pad[N]; };"
    , ""
    , "template<uint8_t POS, uint32_t MASK>"
    , "struct bit_field_t"
    , "{"
    , "    template <uint32_t X>"
    , "    static constexpr uint32_t value()"
    , "    {"
    , "        static_assert((X & ~MASK) == 0, \"field value too large\");"
    , "        return X << POS;"
    , "    }"
    , "};"
    ] ++
    concatMap peripheral ys ++
    concatMap peripheral xs
    where (xs, ys) = partition (isJust . derivedFrom) peripherals

prettyPeripheral :: Peripheral -> [Text]
prettyPeripheral = peripheral

peripheral :: Peripheral -> [Text]
peripheral Peripheral{derivedFrom=Just from,..} =
    [ ""
    , "typedef " <> T.toLower from <> "_t " <> T.toLower name <> "_t;"
    ]
peripheral Peripheral{..} =
    banner [ cleanWords description ] ++
    [ ""
    , "struct " <> T.toLower name <> "_t"
    , "{"
    ] ++
    concatMap (register w) (reserve $ sortOn addressOffset registers) ++
    concatMap registerFields registers ++
    [ "};"
    ]
    where w = maximum [ T.length name | Register{..} <- registers ]

reserve :: [Register] -> [(Register, Maybe Int)]
reserve xs = zip xs $ ys ++ [ Nothing ]
    where ys = map (Just . addressOffset) $ tail xs

register :: Int -> (Register, Maybe Int) -> [Text]
register w (Register{..}, nextOffset) = T.concat
    [ "    "
    , "volatile uint32_t"
    , " "
    , name
    , ";"
    , T.replicate (w - T.length name) " "
    , " // " <> maybe "" (\t -> "[" <> t <> "] ") access
    , cleanWords description
    ] : [ let m = (n - (addressOffset + 4)) `div` 4 in T.concat
          [ "    "
          , "reserved_t<"
          , hex m
          , "> _"
          , hex n
          , ";"
          ]
        | Just n <- [ nextOffset ]
        , n > addressOffset + 4
        ]

registerFields :: Register -> [Text]
registerFields Register{..} = "" : rv : xs
    where xs = concatMap (field w name) fields
          w = maximum $ map fieldWidth fields
          rv = T.concat
            [ "    "
            , "static constexpr uint32_t"
            , " "
            , name
            , "_RESET_VALUE"
            , " = "
            , hex resetValue
            , ";"
            ]

field :: Int -> Text -> Field -> [Text]
field w regName f@Field{..}
    | bitWidth == 32 = []                   -- trivial field
    | bitWidth == 1 = (:[]) $ T.concat      -- single-bit
        [ decl
        , " = "
        , bitConstant bitOffset
        , ";"
        , doc
        ]
    | otherwise =                           -- multi-bit
        [ "    " <> "template<uint32_t X>"
        , decl <> " =  " <> doc
        , "        " <> bits <> ";"
        ]
    where decl = T.concat
              [ "    "
              , "static constexpr uint32_t"
              , " "
              , regName
              , "_"
              , name
              ]
          doc = T.concat
              [ T.replicate (w - fieldWidth f) " "
              , " // "
              , cleanWords description
              ]
          bits = T.concat
              [ "bit_field_t<"
              , T.pack $ show bitOffset
              , ", "
              , hex $ shift 0xffffffff (bitWidth - 32)
              , ">::value<X>()"
              ]

fieldWidth :: Field -> Int
fieldWidth Field{..}
    | bitWidth == 32 = 0
    | bitWidth == 1 = T.length $ name <> bitConstant bitOffset
    | otherwise = T.length $ name

bitConstant :: Int -> Text
bitConstant = hex . shift 1

peripheralMap :: SVD -> [Text]
peripheralMap SVD{..} =
    [ name <> "," <> T.pack (show baseAddress)
    | Peripheral{..} <- sortOn baseAddress peripherals
    ]

cleanWords :: Text -> Text
cleanWords = T.unwords . T.words . T.filter isAscii

{-
type SVD = (Text, [Peripheral])

data Peripheral = Peripheral
    { name          :: Text
    , description   :: Text
    , baseAddress   :: Int
    , interrupts    :: [Interrupt]
    , registers     :: [Register]
    , derivedFrom   :: Maybe Text
    } deriving (Show)

instance Default Peripheral where
    def = Peripheral "" "" 0 [] [] Nothing

data Interrupt = Interrupt
    { name          :: Text
    , description   :: Text
    , value         :: Int
    } deriving (Show)

instance Default Interrupt where
    def = Interrupt "" "" 0

data Register = Register
    { name          :: Text
    , displayName   :: Text
    , description   :: Text
    , addressOffset :: Int
    , size          :: Int
    , access        :: Text
    , resetValue    :: Int
    , fields        :: [Field]
    } deriving (Show)

instance Default Register where
    def = Register "" "" "" 0 0 0 []

data Field = Field
    { name          :: Text
    , description   :: Text
    , bitOffset     :: Int
    , bitWidth      :: Int
    } deriving (Show)

-}
