module HXT
    ( module HXT.Core
    , NTree
    , atTag
    , attrText
    , attrTextMay
    , elemText
    , elemTextMay
    , list
    ) where

import Text.XML.HXT.Core as HXT.Core
import Data.Tree.NTree.TypeDefs (NTree)

atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

attrText :: ArrowXml cat => String -> cat XmlTree String
attrText tag = hasAttr tag >>> getAttrValue tag

attrTextMay :: ArrowXml a => String -> a XmlTree (Maybe String)
attrTextMay tag
    = (hasAttr tag >>> getAttrValue tag >>> arr Just)
    `orElse` constA Nothing

elemText :: ArrowXml cat => String -> cat (NTree XNode) String
elemText tag
    = getChildren
    >>> isElem
    >>> hasName tag
    >>> getChildren
    >>> getText

elemTextMay :: ArrowXml a => String -> a (NTree XNode) (Maybe String)
elemTextMay tag
    = (elemText tag >>> arr Just)
    `orElse` constA Nothing

list :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
list tag
    = getChildren
    >>> isElem
    >>> hasName tag
