{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module PrettySVD (prettySVD, peripheralMap) where

import qualified Data.Text as T
import Data.List (sortOn)
import Data.Bits (shift)
import Numeric (showHex)
import ParseSVD

type Text = T.Text

prettySVD :: SVD -> [Text]
prettySVD SVD{..} =
    [ "#pragma once"
    ] ++
    banner [ name, "Version " <> version ] ++
    [ "#include <stdint.h>"
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
    concatMap peripheral peripherals

peripheral :: Peripheral -> [Text]
peripheral Peripheral{derivedFrom=Just from,..} =
    [ ""
    , "// derived from " <> T.toLower from
    ]
peripheral Peripheral{..} =
    banner [ cleanWords description ] ++
    [ "struct " <> T.toLower name <> "_t"
    , "{"
    ] ++
    map (register w) registers ++
    concatMap registerFields registers ++
    [ "};"
    ]
    where w = maximum [ T.length name | Register{..} <- registers ]

register :: Int -> Register -> Text
register w Register{..} = T.concat
    [ "    "
    , "volatile uint32_t"
    , " "
    , name
    , ";"
    , T.replicate (w - T.length name) " "
    , " // " <> maybe "" (\t -> "[" <> t <> "] ") access
    , cleanWords description
    ]

registerFields :: Register -> [Text]
registerFields Register{..}
    | [] <- xs = []
    | otherwise = "" : xs
    where xs = concatMap (field w name) fields
          w = maximum $ map fieldWidth fields

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
        [ "    " <> "template<uint32_t X>" <> doc
        , decl <> " = " <> bits
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
              , ">::value<X>();"
              ]

fieldWidth :: Field -> Int
fieldWidth Field{..}
    | bitWidth == 32 = 0
    | bitWidth == 1 = T.length $ name <> bitConstant bitOffset
    | otherwise = 0

bitConstant :: Int -> Text
bitConstant = hex . shift 1

banner :: [Text]-> [Text]
banner xs =
    [ ""
    , "////"
    , "//"
    ] ++
    map ("//      " <>) xs ++
    [ "//"
    , "////"
    , ""
    ]

peripheralMap :: SVD -> [Text]
peripheralMap SVD{..} =
    [ name <> "," <> T.pack (show baseAddress)
    | Peripheral{..} <- sortOn baseAddress peripherals
    ]

cleanWords :: Text -> Text
cleanWords = T.unwords . T.words

hex :: Int -> Text
hex x = T.pack $ "0x" ++ showHex x ""

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
