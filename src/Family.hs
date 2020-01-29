{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields #-}
module Family
    ( Family
    , SubFamily
    , MCU(..)
    , Filter(..)
    , parseFamilies
    , prune
    , mcuList
    , buildRules
    ) where

import Text.HTML.TagSoup
import Data.Monoid
import Data.Char (isSpace, toLower)
import Data.Maybe (mapMaybe)
import Data.List (stripPrefix, break, nub, sort)
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Arrow
import IPMode

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

data Filter
    = Family String
    | SubFamily String
    | Package String

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

prune :: [Filter] -> [Family] -> [Family]
prune fs
    = filter (not . null . snd)
    . map (second $ filter (not . null . snd))
    . map (second $ map (second $ filter (mcuPred fs)))
    . map (second $ filter (subFamilyPred fs))
    . filter (familyPred fs)

familyPred :: [Filter] -> Family -> Bool
familyPred fs (name, _) = null xs || name `elem` xs
    where xs = [ x | Family x <- fs ]

subFamilyPred :: [Filter] -> SubFamily -> Bool
subFamilyPred fs (name, _) = null xs || name `elem` xs
    where xs = [ x | SubFamily x <- fs ]

mcuPred :: [Filter] -> MCU -> Bool
mcuPred fs MCU{..} = null xs || package `elem` xs
    where xs = [ x | Package x <- fs ]

mcuList :: [Family] -> IO ()
mcuList families = 
    forM_ families $ \(name, subFamilies) -> do
        putStrLn $ replicate 80 '='
        putStrLn name
        forM_ subFamilies $ \(name, mcus) -> do
            putStrLn $ replicate 80 '-'
            putStrLn $ "    " <> name
            putStrLn $ replicate 80 '-'
            forM_ mcus $ \MCU{..} -> do
                putStrLn $ "        " <> unwords [ name, package, refName, rpn, show flash <> "/" <> show ram ]

buildRules :: [Family] -> [String]
buildRules families =
    [ "module STM32MCUs (MCU(..), mcuList) where"
    , ""
    , "data MCU = MCU"
    , "    { name   :: String"
    , "    , family :: Family"
    , "    , core   :: String"
    , "    , flash  :: Int"
    , "    , ram    :: Int"
    , "    } deriving (Eq, Ord, Show)"
    , ""
    , "mcuList :: [MCU]"
    , "mcuList ="
    ] ++
    zipWith (\x s -> "    " <> s <> " " <> x) xs ("[" : repeat ",") ++
    [ "    ]"
    , ""
    ]
    where xs = nub $ sort
               [ unwords
                    [ "MCU"
                    , show refName
                    , show family
                    , show $ cleanCore core
                    , show flash
                    , show ram
                    ]
               | (family, subFamilies) <- families
               , (subFamily, mcus) <- subFamilies
               , MCU{..} <- mcus
               ]
          cleanCore s | Just r <- stripPrefix "arm " $ map toLower s = r
                      | otherwise = error $ "unexpeced core format: " <> s

