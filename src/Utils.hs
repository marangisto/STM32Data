{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( Text
    , enum
    , banner
    , hex
    , fromHex
    , unPlus
    , cleanWords
    , writeText
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (isAscii)
import Numeric (readHex, showHex)
import System.IO

type Text = T.Text

enum :: Text -> [Text] -> [Text]
enum name xs = concat
    [ [ "", "enum " <> name ]
    , [ s <> x
      | (s, x) <- zip ("    { " : repeat "    , ") xs
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

cleanWords :: Text -> Text
cleanWords = T.unwords . T.words . T.filter isAscii

-- | Does the right thing to not get crlf in output
writeText :: FilePath -> [Text] -> IO ()
writeText fn xs = withFile fn WriteMode $ \f -> do
    hSetNewlineMode f noNewlineTranslation
    T.hPutStr f $ T.unlines xs

