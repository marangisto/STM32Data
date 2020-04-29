{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields #-}
module NormalSVD (normalizeSVD) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
--import qualified Data.Map.Strict as Map
import Data.List (partition, sortOn)
import Data.List.Extra (groupSort)
import Data.Hashable
import Data.Maybe (fromMaybe)
import System.FilePath
import System.Directory
import Control.Monad
import Numeric (showHex)
import ParseSVD
import FixupSVD
import PrettySVD

type Text = T.Text

instance Hashable Peripheral where
    hashWithSalt h Peripheral{..} = hashWithSalt h
        ( sortOn addressOffset registers
        , derivedFrom
        )

instance Hashable Interrupt where
    hashWithSalt h Interrupt{..} = hashWithSalt h
        ( name
        , value
        )

instance Hashable Register where
    hashWithSalt h Register{..} = hashWithSalt h
        ( name
        , addressOffset
        , resetValue
        , sortOn bitOffset fields
        )

instance Hashable Field where
    hashWithSalt h Field{..} = hashWithSalt h
        ( name
        , bitOffset
        , bitWidth
        )

data Normalization
    = Representative
    { svdName   :: Text
    , name      :: Text
    , groupName :: Maybe Text
    , digest    :: Int
    , text      :: FilePath
    }
    | Normalization
    { svdName   :: Text
    , name      :: Text
    , digest    :: Int
    } deriving (Show)

normalizeSVD :: FilePath -> FilePath -> Text -> [(Text, FilePath)] -> IO ()
normalizeSVD tmp dir family xs = do
    putStrLn $ T.unpack family <> " in " <> tmp
    ys <- forM xs $ \(x, fn) -> do
        putStrLn $ "parsing " <> fn
        svd <- parseSVD <$> T.readFile fn
        processSVD tmp family svd
    let (rs, ds) = partition isRep $ concat ys
        gs = groupSort $ map (\r@Representative{..} -> (groupName, r)) rs
    putStrLn $ show (length rs) <> " peripherals, " <> show (length gs) <> " groups"
    dir <- return $ dir </> lower family
    createDirectoryIfMissing False dir
    mapM_ (uncurry $ genHeader dir family) gs
    where isRep Representative{} = True
          isRep _ = False

genHeader :: FilePath -> Text -> Maybe Text -> [Normalization] -> IO ()
genHeader dir family group rs = do
    hs <- forM (sortOn f rs) $ \Representative{..} -> T.readFile text
    let header = dir </> maybe "other" lower group <.> "h"
    putStrLn $ "writing " <> header
    T.writeFile header $ T.concat $ preamble : hs
    where f :: Normalization -> Text
          f = name
          preamble = T.unlines $ "#pragma once" : banner
            [ family <> " " <> fromMaybe "other" group <> " peripherals"
            ]

processSVD :: FilePath -> Text -> SVD -> IO [Normalization]
processSVD tmp family SVD{..} = do
    putStrLn $ "processing " <> T.unpack name
    mapM (processPeripheral tmp family name)
        [ p
        | p@Peripheral{..} <- peripherals
        , Nothing <- [ derivedFrom ]
        ]

processPeripheral :: FilePath -> Text -> Text -> Peripheral -> IO Normalization
processPeripheral tmp family svdName p@Peripheral{..} = do
    let h = hash p
        fn = tmp </> T.unpack (hex (abs h)) <.> "h"
    already <- doesFileExist fn
    if already then do
        putStrLn $ T.unpack name <> " normalized"
        return Normalization{digest=h,..}
    else do
        putStr $ T.unpack name <> fn <> "..."
        T.writeFile fn
            $ T.unlines
            . prettyPeripheral
            . qualify svdName
            $ fixupPeripheral family p
        putStrLn $ "done"
        return Representative{digest=h,text=fn,..}

qualify :: Text -> Peripheral -> Peripheral
qualify svdName p@Peripheral{..} = p { name = svdName <> "_" <> name }

hex :: Int -> Text
hex x = T.pack $ "0x" ++ showHex x ""

lower :: Text -> String
lower = T.unpack . T.toLower

