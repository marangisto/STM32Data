module TagSoup
    ( Text
    , Tag(..)
    , parseTags
    , fromTagText
    , fromAttrib
    , innerText
    , isTagOpen
    , isTagClose
    , isTagOpenName
    , isTagCloseName
    , partitions
    , (~==)
    , (~/=)
    ) where

import qualified Text.HTML.TagSoup as T
import Text.HTML.TagSoup hiding ((~==), (~/=))
import qualified Data.Text as T

type Text = T.Text

(~==) :: Tag Text -> String -> Bool
(~==) a b = (T.~==) a (b :: String)

(~/=) :: Tag Text -> String -> Bool
(~/=) a b = (T.~/=) a (b :: String)

