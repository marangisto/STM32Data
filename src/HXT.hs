module HXT
    ( module HXT.Core
    , atTag
    , attrText
    , attrTextMay
    , elemText
    , elemTextMay
    , list
    ) where

import Text.XML.HXT.Core as HXT.Core

atTag tag = deep (isElem >>> hasName tag)

attrText tag = hasAttr tag >>> getAttrValue tag

attrTextMay tag
    = (hasAttr tag >>> getAttrValue tag >>> arr Just)
    `orElse` constA Nothing

elemText tag
    = getChildren
    >>> isElem
    >>> hasName tag
    >>> getChildren
    >>> getText

elemTextMay tag
    = (elemText tag >>> arr Just)
    `orElse` constA Nothing

list tag
    = getChildren
    >>> isElem
    >>> hasName tag
