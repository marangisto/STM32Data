{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Family
    ( Family
    , SubFamily
    , Peripheral
    , Controller(..)
    , Filter(..)
    , parseFamilies
    , prune
    , flatten
    , mcuList
    , controllers
    , preAmble
    , buildRules
    ) where

import Data.Text (Text, pack, unpack, toUpper, toLower, stripPrefix)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (nub, sort)
import qualified Data.Text as T
import Control.Monad
import Control.Arrow
import HXT

type Family = (Text, [SubFamily])

type SubFamily = (Text, [Controller])

data Controller = Controller
    { name          :: !Text
    , package       :: !Text
    , refName       :: !Text
    , rpn           :: !Text
    , core          :: !Text
    , frequency     :: !Int
    , flash         :: !Int
    , ram           :: !Int
    , numIO         :: !Int
    , voltage       :: !(Double, Double)
    , current       :: !(Maybe Double, Maybe Double)
    , temperature   :: !(Maybe (Double, Double))
    , peripherals   :: ![(Peripheral, Int)]
    }
    deriving (Show)

type Peripheral = Text

parseFamilies :: FilePath -> IO [Family]
parseFamilies fn = do
    s <- readFile fn
    runX (readString [ withValidate yes ] s >>> getFamily)

getFamily = atTag "Family" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    subFamilies <- listA getSubFamily -< x
    returnA -< (name, subFamilies)

getSubFamily = atTag "SubFamily" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    mcus <- listA getMCU -< x
    returnA -< (name, mcus)

getMCU = atTag "Mcu" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    package <- arr pack <<< attrText "PackageName" -< x
    refName <- arr pack <<< attrText "RefName" -< x
    rpn <- arr pack <<< attrText "RPN" -< x
    core <- arr pack <<< elemText "Core" -< x
    frequency <- arr read <<< elemText "Frequency" -< x
    flash <- arr read <<< elemText "Flash" -< x
    ram <- arr read <<< elemText "Ram" -< x
    numIO <- arr read <<< elemText "IONb" -< x
    voltage <- getVoltage -< x
    current <- getCurrent `orElse` constA (Nothing, Nothing) -< x
    temperature <- ( arr Just <<< getTemperature
                   ) `orElse` constA Nothing -< x
    peripherals <- listA getPeripheral -< x
    returnA -< Controller{..}

getVoltage = atTag "Voltage" >>> proc x -> do
    min <- arr read <<< attrText "Min" -< x
    max <- arr read <<< attrText "Max" -< x
    returnA -< (min, max)

getCurrent = atTag "Current" >>> proc x -> do
    low <- arr (fmap read) <<< attrTextMay "Lowest" -< x
    run <- arr (fmap read) <<< attrTextMay "Run" -< x
    returnA -< (low, run)

getTemperature = atTag "Temperature" >>> proc x -> do
    min <- arr read <<< attrText "Min" -< x
    max <- arr read <<< attrText "Max" -< x
    returnA -< (min, max)

getPeripheral = atTag "Peripheral" >>> proc x -> do
    name <- arr pack <<< attrText "Type" -< x
    count <- arr read <<< attrText "MaxOccurs" -< x
    returnA -< (name, count)

data Filter
    = Family Text
    | SubFamily Text
    | Package Text

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

mcuPred :: [Filter] -> Controller -> Bool
mcuPred fs Controller{..} = null xs || package `elem` xs
    where xs = [ x | Package x <- fs ]

flatten :: [Family] -> [(Text, Text, Controller)]
flatten families = 
    [ (family, subFamily, controller)
    | (family, subFamilies) <- families
    , (subFamily, controllers) <- subFamilies
    , controller <- controllers
    ]

mcuList :: [Family] -> IO ()
mcuList families = 
    forM_ families $ \(name, subFamilies) -> do
        putStrLn $ replicate 80 '='
        putStrLn $ unpack name
        forM_ subFamilies $ \(name, mcus) -> do
            putStrLn $ replicate 80 '-'
            putStrLn $ "    " <> unpack name
            putStrLn $ replicate 80 '-'
            forM_ mcus $ \Controller{..} -> do
                putStrLn $ "        " <> unwords
                    [ unpack name
                    , unpack package
                    , unpack refName
                    , unpack rpn
                    , show flash <> "/" <> show ram
                    ]

controllers :: [SubFamily] -> [Controller]
controllers subFamilies = [ c | (_, cs) <- subFamilies, c <- cs ]

preAmble :: Controller -> [Text]
preAmble Controller{..} =
    [ "#pragma once"
    , ""
    , "###"
    , "#"
    , "#        " <> refName
    , "#"
    ] ++ map fmt
    [ ("core", core)
    , ("package", package)
    , ("frequency", pack $ show frequency)
    , ("flash", pack $ show flash <> "kB")
    , ("ram", pack $ show ram <> "kB")
    , ("IO count", pack $ show numIO)
    ] ++ map (fmt . second (pack . show)) peripherals ++
    [ "#"
    , "###"
    , ""
    ]
    where fmt :: (Text, Text) -> Text
          fmt (l, s) = "#        " <> l
                    <> pack (replicate (12 - (length $ unpack l)) ' ')
                    <> ": " <> s

buildRules :: [Family] -> [Text]
buildRules families =
    [ "module STM32 (MCU(..), mcuList) where"
    , ""
    , "data MCU = MCU"
    , "    { name   :: String"
    , "    , family :: String"
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
               [ pack $ unwords
                    [ "MCU"
                    , show refName
                    , show family
                    , show $ cleanCore core
                    , show flash
                    , show ram
                    ]
               | (family, subFamilies) <- families
               , (_, mcus) <- subFamilies
               , Controller{..} <- mcus
               ]
          cleanCore s | Just r <- stripPrefix "arm " $ toLower s = r
                      | otherwise = error $ unpack $ "unexpected core format: " <> s

