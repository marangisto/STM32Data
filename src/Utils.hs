{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( Text
    , enum
    , banner
    , hex
    , writeText
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Numeric (showHex)

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

-- | Does the right thing to not get crlf in output
writeText :: FilePath -> [Text] -> IO ()
writeText fn xs = withFile fn WriteMode $ \f -> do
    hSetNewlineMode f noNewlineTranslation
    T.hPutStr f $ T.unlines xs

