{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module PrettySVD (prettySVD, peripheralMap) where

import qualified Data.Text as T
import Data.List (sortOn)
import ParseSVD

type Text = T.Text

prettySVD :: SVD -> [Text]
prettySVD SVD{..} =
    [ "#pragma once"
    ] ++
    banner [ name, description, "Version: " <> version ] ++
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
    banner [ (T.unwords $ T.words description) ] ++
    [ "struct " <> T.toLower name <> "_t"
    , "{"
    ] ++
    map (register w) registers ++
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
    , description
    ]

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
