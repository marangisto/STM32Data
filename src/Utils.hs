{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( Text
    , enum
    , banner
    , hex
    ) where

import qualified Data.Text as T
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

