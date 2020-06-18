{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( Text
    , enum
    , enum2
    , banner
    , hex
    , fromHex
    , unPlus
    , packUpper
    , packWords
    , cleanWords
    , writeText
    , traverseDir
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (isAscii)
import Numeric (readHex, showHex)
import System.IO
import System.Directory
import System.FilePath
import Control.Monad.Extra
import Control.Monad

type Text = T.Text

enum :: Text -> [Text] -> [Text]
enum name xs = concat
    [ [ "", "enum " <> name ]
    , [ s <> x
      | (s, x) <- zip ("    { " : repeat "    , ") xs
      ]
    , [ "    };" ]
    ]

enum2 :: Text -> [(Text, Int)] -> [Text]
enum2 name xs = concat
    [ [ "", "enum " <> name ]
    , [ s <> x <> " = " <> T.pack (show i)
      | (s, (x, i)) <- zip ("    { " : repeat "    , ") xs
      ]
    , [ "    };" ]
    ]

banner :: [Text]-> [Text]
banner xs =
    [ ""
    , "////"
    , "//"
    ] ++
    map ("//      " <>) xs ++
    [ "//"
    , "////"
    ]

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

-- | Does the right thing to not get crlf in output
writeText :: FilePath -> [Text] -> IO ()
writeText fn xs = withFile fn WriteMode $ \f -> do
    hSetNewlineMode f noNewlineTranslation
    T.hPutStr f $ T.unlines xs

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

