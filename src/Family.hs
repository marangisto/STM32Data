{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields, OverloadedStrings #-}
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

import Text.Read (readMaybe)
import Data.Char (toLower)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (nub, sort)
import qualified Data.Text as T
import Control.Monad
import Control.Arrow
import TagSoup

type Family = (Text, [SubFamily])

type SubFamily = (Text, [Controller])

data Controller = Controller
    { name          :: Text
    , package       :: Text
    , refName       :: Text
    , rpn           :: Text
    , core          :: Text
    , frequency     :: Int
    , flash         :: Int
    , ram           :: Int
    , numIO         :: Int
    , peripherals   :: [(Peripheral, Int)]
    }
    deriving (Show)

type Peripheral = Text

data Filter
    = Family Text
    | SubFamily Text
    | Package Text

elementText :: String -> [Tag Text] -> Text
elementText s ts
    | (_:y:_) <- dropWhile (~/=s) ts = fromTagText y
    | otherwise = ""

peripheralFromTag :: Tag Text -> (Text, Int)
peripheralFromTag t = (fromAttrib "Type" t, read $ T.unpack $ fromAttrib "MaxOccurs" t)

mcuFromTags :: [Tag Text] -> Maybe Controller
mcuFromTags (t:ts)
    | fromAttrib "Visible" t == "false" = Nothing
    | otherwise = Just Controller{..}
    where name = fromAttrib "Name" t
          package = fromAttrib "PackageName" t
          refName = fromAttrib "RefName" t
          rpn = fromAttrib "RPN" t  -- this seems to be the 'real' name
          core = elementText "<Core>" ts
          frequency = fromMaybe 0 $ readMaybe $ T.unpack $ elementText "<Frequency>" ts
          flash = read $ T.unpack $ elementText "<Flash>" ts
          ram = read $ T.unpack $ elementText "<Ram>" ts
          numIO = read $ T.unpack $ elementText "<IONb>" ts
          peripherals = map peripheralFromTag $ filter (~=="<Peripheral>") ts
mcuFromTags [] = Nothing

subFamilyFromTags :: [Tag Text] -> SubFamily
subFamilyFromTags (t:ts) = (fromAttrib "Name" t, xs)
    where xs = mapMaybe mcuFromTags
             $ partitions (~=="<Mcu>") ts
subFamilyFromTags [] = error "empty sub-family"

familyFromTags :: [Tag Text] -> Family
familyFromTags (t:ts) = (fromAttrib "Name" t, xs)
    where xs = map subFamilyFromTags
             $ partitions (~=="<SubFamily>") ts
familyFromTags [] = error "empty sub-family"

parseFamilies :: Text -> [Family]
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
        putStrLn $ T.unpack name
        forM_ subFamilies $ \(name, mcus) -> do
            putStrLn $ replicate 80 '-'
            putStrLn $ "    " <> T.unpack name
            putStrLn $ replicate 80 '-'
            forM_ mcus $ \Controller{..} -> do
                putStrLn $ "        " <> unwords
                    [ T.unpack name
                    , T.unpack package
                    , T.unpack refName
                    , T.unpack rpn
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
    , ("frequency", T.pack $ show frequency)
    , ("flash", T.pack $ show flash <> "kB")
    , ("ram", T.pack $ show ram <> "kB")
    , ("IO count", T.pack $ show numIO)
    ] ++ map (fmt . second (T.pack . show)) peripherals ++
    [ "#"
    , "###"
    , ""
    ]
    where fmt :: (Text, Text) -> Text
          fmt (l, s) = "#        "<> l <> T.pack (replicate (12 - T.length l) ' ') <> ": " <> s

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
               [ T.pack $ unwords
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
          cleanCore s | Just r <- T.stripPrefix "arm " $ T.map toLower s = r
                      | otherwise = error $ T.unpack $ "unexpected core format: " <> s

