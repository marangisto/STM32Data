{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( Text
    , hex
    , fromHex
    , unPlus
    , packUpper
    , packWords
    , cleanWords
    , cleanName
    , writeText
    , traverseDir
    , cacheLines
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Char (isAscii)
import Data.Hashable
import Numeric (readHex, showHex)
import System.IO
import System.Directory
import System.FilePath
import Control.Monad.Extra

type Text = T.Text

hex :: Int -> Text
hex x = T.pack $ "0x" ++ showHex x ""

fromHex :: Text -> Int
fromHex t
    | ('0':'x':xs) <- s, [(n, "")] <- readHex xs = n
    | ('0':'X':xs) <- s, [(n, "")] <- readHex xs = n
    | [(n, "")] <- readHex s = n
    | otherwise = error $ "failed or read hex '" <> s <> "'"
    where s = T.unpack t

unPlus :: Text -> Text
unPlus s
    | Just x <- T.stripSuffix "+" s = x <> "Plus"
    | otherwise = s

packUpper :: String -> Text
packUpper = T.toUpper . T.pack

packWords :: String -> Text
packWords = cleanWords . T.pack

cleanWords :: Text -> Text
cleanWords = T.unwords . T.words . T.filter isAscii

cleanName :: Text -> Text
cleanName = T.map (\c -> if c == '-' then '_' else c)

-- | Does the right thing to not get crlf in output
writeText :: FilePath -> TL.Text -> IO ()
writeText fn x = withFile fn WriteMode $ \f -> do
    putStrLn fn
    hSetNewlineMode f noNewlineTranslation
    T.hPutStr f $ TL.toStrict x

traverseDir
    :: (FilePath -> Bool)       -- directory selector
    -> (b -> FilePath -> IO b)  -- transition function
    -> b
    -> FilePath
    -> IO b
traverseDir validDir transition = go
    where go state dirPath = do
              names <- listDirectory dirPath
              let paths = map (dirPath </>) names
              (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
              state' <- foldM transition state filePaths
              foldM go state' (filter validDir dirPaths)

cacheLines :: (FilePath -> IO [String]) -> FilePath -> IO [String]
cacheLines act fp = do
    dir <- getTemporaryDirectory
    let fn = dir </> showHex (abs $ hash fp) "" <.> "tmp"
    b <- doesFileExist fn
    if b then lines <$> readFile fn else do
        xs <- act fp
        writeFile fn $ unlines xs
        putStrLn $ "cached " <> fp <> " in " <> fn
        return xs

