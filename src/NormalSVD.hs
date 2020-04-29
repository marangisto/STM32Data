{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields #-}
module NormalSVD (normalizeSVD) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (partition, sortOn)
import Data.List.Extra (groupSort)
import Data.Hashable
import Data.Maybe (fromMaybe, mapMaybe)
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
    | Duplicate
    { svdName   :: Text
    , name      :: Text
    , digest    :: Int
    }
    | Derived
    { svdName       :: Text
    , name          :: Text
    , derivedFrom   :: Text
    } deriving (Show)

normalizeSVD :: FilePath -> FilePath -> Text -> [(Text, FilePath)] -> IO ()
normalizeSVD tmp dir family xs = do
    putStrLn $ T.unpack family <> " in " <> tmp
    ys <- fmap concat $ forM xs $ \(x, fn) -> do
        putStrLn $ "parsing " <> fn
        SVD{..} <- parseSVD <$> T.readFile fn
        mapM (processPeripheral tmp family name) peripherals
    let ds = groupSort $ map (remap $ digests ys) ys
        gs = groupSort
            [ (groupName, (r, fromMaybe [] $ lookup digest ds))
            | r@Representative{..} <- ys
            ]
    dir <- return $ dir </> lower family
    createDirectoryIfMissing False dir
    mapM_ (uncurry $ genHeader dir family) gs

genHeader
    :: FilePath
    -> Text
    -> Maybe Text
    -> [(Normalization, [(Text, Text)])]
    -> IO ()
genHeader dir family group rs = do
    hs <- forM (sortOn f rs) $ \(Representative{..}, _) -> T.readFile text
    let header = dir </> maybe "other" lower group <.> "h"
    putStrLn $ "writing " <> header
    T.writeFile header $ T.unlines $ concat
        [ preamble
        , (:[]) $ T.concat hs
        , concatMap genTraits rs
        ]
    where f :: (Normalization, [(Text, Text)]) -> Text
          f (Representative{..}, _) = name
          preamble = "#pragma once" : banner
            [ family <> " " <> fromMaybe "other" group <> " peripherals"
            ]

genTraits :: (Normalization, [(Text, Text)]) -> [Text]
genTraits (Representative{..}, xs) = map f xs
    where f (s, n) = T.toLower $ T.concat
            [ "typedef"
            , " "
            , svdName <> "_" <> name <> "_t"
            , " "
            , n <> "_t"
            , "; // "
            , s
            ]

processPeripheral
    :: FilePath
    -> Text
    -> Text
    -> Peripheral
    -> IO Normalization
processPeripheral tmp family svdName p@Peripheral{derivedFrom=Just s,..} =
    return Derived{derivedFrom=s,..}
processPeripheral tmp family svdName p@Peripheral{..} = do
    let h = hash p
        fn = tmp </> T.unpack (hex (abs h)) <.> "h"
    already <- doesFileExist fn
    if already then do
        putStrLn $ T.unpack name <> " normalized"
        return Duplicate{digest=h,..}
    else do
        putStr $ T.unpack name <> fn <> "..."
        T.writeFile fn
            $ T.unlines
            . prettyPeripheral
            . qualify svdName
            $ fixupPeripheral family p
        putStrLn $ "done"
        return Representative{digest=h,text=fn,..}

digests :: [Normalization] -> [((Text, Text), Int)]
digests = mapMaybe f
    where f Representative{..} = Just ((svdName, name), digest)
          f Duplicate{..} = Just ((svdName, name), digest)
          f _ = Nothing

remap :: [((Text, Text), Int)] -> Normalization -> (Int, (Text, Text))
remap _ Representative{..} = (digest, (svdName, name))
remap _ Duplicate{..} = (digest, (svdName, name))
remap ss Derived{..} = (digest, (svdName, name))
    where digest = fromMaybe (error $ "failed to derive " <> qname)
                 $ lookup (svdName, derivedFrom) ss
          qname = T.unpack $ svdName <> "." <> derivedFrom

qualify :: Text -> Peripheral -> Peripheral
qualify svdName p@Peripheral{..} = p { name = svdName <> "_" <> name }

hex :: Int -> Text
hex x = T.pack $ "0x" ++ showHex x ""

lower :: Text -> String
lower = T.unpack . T.toLower

