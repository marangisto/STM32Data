{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, OverloadedStrings, DuplicateRecordFields #-}
module NormalSVD (normalizeSVD) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
--import qualified Data.Map.Strict as Map
import Data.List (sortOn)
--import Data.List.Extra (groupSort)
import Data.Hashable
import System.FilePath
import System.Directory
import Control.Monad
import Numeric (showHex)
import ParseSVD
import PrettySVD

type Text = T.Text

instance Hashable Peripheral where
    hashWithSalt h Peripheral{..} = hashWithSalt h
--        ( name
--        , sortOn value interrupts
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

normalizeSVD :: FilePath -> Text -> [(Text, FilePath)] -> IO ()
normalizeSVD tmp family xs = do
    putStrLn $ T.unpack family <> " in " <> tmp
    ys <- forM xs $ \(x, fn) -> do
        putStrLn $ "parsing " <> fn
        svd <- parseSVD <$> T.readFile fn
        processSVD tmp svd
    mapM_ print $ concat ys
    let ps = [ text | Representative{..} <- concat ys ]
    putStrLn $ "number of peripherals = " <> show (length ps)

data Normalization
    = Representative
    { svdName   :: Text
    , name      :: Text
    , digest    :: Int
    , text      :: FilePath
    }
    | Normalization
    { svdName   :: Text
    , name      :: Text
    , digest    :: Int
    } deriving (Show)

processSVD :: FilePath -> SVD -> IO [Normalization]
processSVD tmp SVD{..} = do
    putStrLn $ "processing " <> T.unpack name
    mapM (processPeripheral tmp name)
        [ p
        | p@Peripheral{..} <- peripherals
        , Nothing <- [ derivedFrom ]
        ]

processPeripheral :: FilePath -> Text -> Peripheral -> IO Normalization
processPeripheral tmp svdName p@Peripheral{..} = do
    let h = hash p
        fn = tmp </> T.unpack (hex (abs h)) <.> "h"
    already <- doesFileExist fn
    if already then do
        putStrLn $ T.unpack name <> " normalized"
        return Normalization{digest=h,..}
    else do
        putStr $ T.unpack name <> fn <> "..."
        T.writeFile fn $ T.unlines $ prettyPeripheral p
        putStrLn $ "done"
        return Representative{digest=h,text=fn,..}

hex :: Int -> Text
hex x = T.pack $ "0x" ++ showHex x ""

