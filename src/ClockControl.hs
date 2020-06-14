{-# LANGUAGE RecordWildCards, OverloadedStrings, TupleSections #-}
module ClockControl
    ( ClockControl(..)
    , clockControl
    , clockControlMissing
    , clockControlUnused
    , rccFlags
    , prettyRCC
    ) where

import Control.Arrow (second)
import qualified Data.Text as T
import Data.List (nub, sort)
import Data.List.Extra (groupSort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as Map
import Normalize
import ParseSVD
import Utils

data ClockControl = ClockControl
    { enable        :: !(Maybe RegFlag)
    , enableSM      :: !(Maybe RegFlag)
    , reset         :: !(Maybe RegFlag)
    } deriving (Show)

type RegFlag = (Text, Text)

clockControl :: NormalSVD -> Map Text ClockControl
clockControl
    = fromList
    . map (uncurry mkCC)
    . groupSort
    . nub
    . sort
    . concatMap rccRegFlags
    . rccPeripherals

mkCC :: Text -> [(Text, (Text, Text))] -> (Text, ClockControl)
mkCC periphName xs = (rename periphName, ClockControl{..})
    where enable = lookup "enable" xs
          enableSM = lookup "enableSM" xs
          reset = lookup "reset" xs

rccRegFlags :: PeriphType -> [(Text, (Text, (Text, Text)))]
rccRegFlags PeriphType{..} =
    [ (periphName, (method, (regName, fldName)))
    | Register{name=regName,..} <-registers
    , any (`T.isPrefixOf` regName) [ "AHB", "APB", "IOP" ]
    , Field{name=fldName} <- fields
    , Just (periphName, method) <- [ decode fldName ]
    ]

rccPeripherals :: NormalSVD -> [PeriphType]
rccPeripherals = filter p . periphTypes
    where p PeriphType{typeRef=PeriphRef{..}} = name == "RCC"

rename :: Text -> Text
rename x
    | Just y <- T.stripPrefix "IOP" x = "GPIO" <> y
    | otherwise = x

clockControlMissing :: NormalSVD -> Map Text ClockControl -> [Text]
clockControlMissing nsvd ccs = mapMaybe f $ peripheralNames nsvd
    where f x = maybe (Just x) (const Nothing) $ Map.lookup x ccs

clockControlUnused :: NormalSVD -> Map Text ClockControl -> [Text]
clockControlUnused nsvd ccs = Map.keys ccs'
    where ccs' = foldr Map.delete ccs $ peripheralNames nsvd

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

decode :: Text -> Maybe (Text, Text)
decode field
    | Just x <- T.stripSuffix "SMEN" field = Just (f x, "enableSM")
    | Just x <- T.stripSuffix "EN" field = Just (f x, "enable")
    | Just x <- T.stripSuffix "RST" field = Just (f x, "reset")
    | otherwise = Nothing
    where f x -- | Just y <- T.stripPrefix "IOP" x = "GPIO" <> y
              | otherwise = x

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
prettyClockControl _ "SRAM1" _ = []     -- don't know how to handle
prettyClockControl _ "SRAM2" _ = []     -- don't know how to handle
prettyClockControl _ "SRAM3" _ = []     -- don't know how to handle
prettyClockControl _ "RTCAPB" _ = []    -- don't know how to handle
prettyClockControl _ "MATRIX" _ = []    -- don't know how to handle
prettyClockControl svd periph xs =
    [ ""
    , "template<typename RCC>"
    , "struct clock_control_t<RCC, peripheral_t<"
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
    , "RCC::T::"
    , reg
    , "_" 
    , field 
    , ";"
    , " }"
    ]

