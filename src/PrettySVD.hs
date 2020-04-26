{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields, OverloadedStrings #-}
module PrettySVD (prettySVD) where

import qualified Data.Text as T
import ParseSVD

type Text = T.Text

prettySVD :: SVD -> [Text]
prettySVD (name, ps) =
    [ "#pragma once"
    ] ++
    banner name ++
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
    concatMap peripheral ps

peripheral :: Peripheral -> [Text]
peripheral Peripheral{derivedFrom=Just from,..} =
    [ ""
    , "// derived from " <> T.toLower from
    ]
peripheral Peripheral{..} =
    banner (T.unwords $ T.words description) ++
    [ "struct " <> T.toLower name <> "_t"
    , "{"
    ] ++
    map register registers ++
    [ "};"
    ]

register :: Register -> Text
register Register{..} = T.concat
    [ "    "
    , "volatile uint32_t"
    , "    "
    , name
    , ";"
    , T.replicate (21 - T.length name) " "
    , "// ["
    , access
    , "] "
    , description
    ]

banner :: Text -> [Text]
banner x =
    [ ""
    , "////"
    , "//"
    , "//      " <> x
    , "//"
    , "////"
    , ""
    ]

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
