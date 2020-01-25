{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Family (Family, SubFamily, MCU(..), parseFamilies) where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import PinMode


type Family = (String, [SubFamily])

type SubFamily = (String, [MCU])

data MCU = MCU
    { name          :: String
    , package       :: String
    , refName       :: String
    , rpn           :: String
    , core          :: String
    , frequency     :: Int
    , flash         :: Int
    , ram           :: Int
    , numIO         :: Int
    , peripherals   :: [(Peripheral, Int)]
    }
    deriving (Show)

type Peripheral = String

elementText :: String -> [Tag String] -> String
elementText s ts
    | (_:y:_) <- dropWhile (~/=s) ts = fromTagText y
    | otherwise = ""

peripheralFromTag :: Tag String -> (String, Int)
peripheralFromTag t = (fromAttrib "Type" t, read $ fromAttrib "MaxOccurs" t)

mcuFromTags :: [Tag String] -> Maybe MCU
mcuFromTags (t:ts)
    | fromAttrib "Visible" t == "false" = Nothing
    | otherwise = Just MCU{..}
    where name = fromAttrib "Name" t
          package = fromAttrib "PackageName" t
          refName = fromAttrib "RefName" t
          rpn = fromAttrib "RPN" t  -- this seems to be the 'real' name
          core = elementText "<Core>" ts
          frequency = read $ elementText "<Frequency>" ts
          flash = read $ elementText "<Flash>" ts
          ram = read $ elementText "<Ram>" ts
          numIO = read $ elementText "<IONb>" ts
          peripherals = map peripheralFromTag $ filter (~=="<Peripheral>") ts

subFamilyFromTags :: [Tag String] -> SubFamily
subFamilyFromTags (t:ts) = (fromAttrib "Name" t, xs)
    where xs = mapMaybe mcuFromTags
             $ partitions (~=="<Mcu>") ts

familyFromTags :: [Tag String] -> Family
familyFromTags (t:ts) = (fromAttrib "Name" t, xs)
    where xs = map subFamilyFromTags
             $ partitions (~=="<SubFamily>") ts

parseFamilies :: String -> [Family]
parseFamilies
    = map familyFromTags
    . partitions (~=="<Family>")
    . dropWhile (~/="<Family>")
    . parseTags

