{-# LANGUAGE RecordWildCards, OverloadedStrings, TupleSections #-}
module ClockControl (rccFlags, prettyRCC) where

import Control.Arrow (second)
import qualified Data.Text as T
import Data.List (sort)
import Data.List.Extra (groupSort)
import ParseSVD
import Utils

rccFlags :: Text -> Peripheral -> Maybe (Text, [(Text, Text)])
rccFlags svd Peripheral{name="RCC",..} = Just . (svd,) $
    [ (regName, fldName)
    | Register{name=regName,..} <-registers
    , any (`T.isPrefixOf` regName) [ "AHB", "APB", "IOP" ]
    , Field{name=fldName} <- fields
    ]
rccFlags _ _ = Nothing

prettyRCC :: Text -> [(Text, Text)] -> [Text]
prettyRCC svd regFields
    = concatMap (uncurry $ prettyClockControl svd) xs
    where xs = map (second sort) $ groupSort
               [ (perip, (op, method, reg, field))
               | (reg, field) <- regFields
               , (perip, method, op) <- decodeField field
               ]

data BitOp = Set | Clear deriving (Eq, Ord, Show)

decodeField :: Text -> [(Text, Text, BitOp)]
decodeField field
    | Just x <- T.stripSuffix "SMEN" field =
        [ (f x, "enable_sleep_mode", Set)
        , (f x, "disable_sleep_mode", Clear)
        ]
    | Just x <- T.stripSuffix "EN" field =
        [ (f x, "enable", Set)
        , (f x, "disable", Clear)
        ]
    | Just x <- T.stripSuffix "RST" field =
        [ (f x, "reset", Set)
        ]
    | otherwise = []
    where f x | Just y <- T.stripPrefix "IOP" x = "GPIO" <> y
              | otherwise = x

prettyClockControl
    :: Text                         -- ^ svd
    -> Text                         -- ^ peripheral
    -> [(BitOp, Text, Text, Text)]  -- ^ [(op, method, reg, field)]
    -> [Text]   
prettyClockControl _ "CEC" _ = []       -- FIXME: still an issue?
prettyClockControl _ "SRAM" _ = []      -- don't know how to handle
prettyClockControl _ "RTCAPB" _ = []    -- don't know how to handle
prettyClockControl svd periph xs =
    [ ""
    , "template<>"
    , "struct clock_control_t<peripheral_t<"
    <> svd <> ", " <> f periph <> ">>"
    , "{"
    ] ++
    map (rccMethod svd) xs ++
    [ "};"
    ]
    where f "FLITF" = "FLASH"
          f x = x

rccMethod :: Text -> (BitOp, Text, Text, Text) -> Text
rccMethod svd (op, method, reg, field) = T.concat
    [ "    "
    , "static void " <> method <> "() { "
    , "rcc_t::V."
    , reg
    , if op == Set then " |= " else " &= ~"
    , "rcc_t::T::"
    , reg
    , "_" 
    , field 
    , ";"
    , " }"
    ]

