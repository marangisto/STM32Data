{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
module FrontEnd.ParseDefMapping
    ( DefMapping(..)
    , parseDefMapping
    ) where

import Data.Text (Text, pack)
import HXT

data DefMapping = DefMapping
    { name              :: !Text
    , newName           :: !Text
    , newValue          :: !Text
    , _type             :: !Text
    , value             :: !Text
    } deriving (Show)

parseDefMapping :: FilePath -> IO [DefMapping]
parseDefMapping fn = do
    s <- readFile fn
    runX (readString [ withValidate yes ] s >>> getDefMapping)

getDefMapping = atTag "Item" >>> proc x -> do
    name <- arr pack <<< attrText "Name" -< x
    newName <- arr pack <<< attrText "NewName" -< x
    newValue <- arr pack <<< attrText "NewValue" -< x
    _type <- arr pack <<< attrText "Type" -< x
    value <- arr pack <<< attrText "Value" -< x
    returnA -< DefMapping{..}

