{-# LANGUAGE RecordWildCards, OverloadedStrings, TupleSections #-}
module FrontEnd.ClockControl
    ( ClockControl(..)
    , CCMap
    , resolveCC
    , missingCC
    , unusedCC
    ) where

import qualified Data.Text as T
import Data.List (nub, sort)
import Data.List.Extra (groupSort)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as Map
import FrontEnd.Normalize
import Utils

type CCMap = Map Text ClockControl

data ClockControl = ClockControl
    { enable        :: !(Maybe RegFlag)
    , enableSM      :: !(Maybe RegFlag)
    , reset         :: !(Maybe RegFlag)
    } deriving (Show)

type RegFlag = (Text, Text)

resolveCC :: NormalSVD () a -> NormalSVD CCMap a
resolveCC x@NormalSVD{..} = x { clockControl = resolveCC' x }

resolveCC' :: NormalSVD () a -> CCMap
resolveCC'
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

rccRegFlags :: PeriphType a -> [(Text, (Text, (Text, Text)))]
rccRegFlags PeriphType{..} =
    [ (periphName, (method, (regName, fldName)))
    | Right Register{name=regName,..} <- registers
    , any (`T.isPrefixOf` regName) [ "AHB", "APB", "IOP" ]
    , Field{name=fldName} <- fields
    , Just (periphName, method) <- [ decode fldName ]
    ]

rccPeripherals :: NormalSVD a b -> [PeriphType b]
rccPeripherals = filter p . periphTypes
    where p PeriphType{typeRef=PeriphRef{..}} = name == "RCC"

rename :: Text -> Text
rename x
    | Just y <- T.stripPrefix "IOP" x = "GPIO" <> y
    | otherwise = x

missingCC :: NormalSVD CCMap b -> [Text]
missingCC nsvd@NormalSVD{..} = mapMaybe f $ peripheralNames nsvd
    where f x = maybe (Just x) (const Nothing) $ Map.lookup x clockControl

unusedCC :: NormalSVD CCMap b -> [Text]
unusedCC nsvd@NormalSVD{..} = Map.keys ccs'
    where ccs' = foldr Map.delete clockControl $ peripheralNames nsvd

decode :: Text -> Maybe (Text, Text)
decode field
    | Just x <- T.stripSuffix "SMEN" field = Just (x, "enableSM")
    | Just x <- T.stripSuffix "EN" field = Just (x, "enable")
    | Just x <- T.stripSuffix "RST" field = Just (x, "reset")
    | otherwise = Nothing

