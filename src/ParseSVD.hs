{-# LANGUAGE RecordWildCards, TupleSections, DuplicateRecordFields, OverloadedStrings #-}
module ParseSVD
    ( SVD(..)
    , Peripheral(..)
    , Register(..)
    , Field(..)
    , parseSVD
    ) where

import Data.List (break)
import qualified Data.Text as T
import Numeric (readHex)
import Data.Default
import TagSoup

type SVD = (Text, [Peripheral])

data Peripheral = Peripheral
    { name          :: Text
    , description   :: Text
    , baseAddress   :: Int
    , registers     :: [Register]
    } deriving (Show)

instance Default Peripheral where
    def = Peripheral "" "" 0 []

data Register = Register
    { name          :: Text
    , displayName   :: Text
    , description   :: Text
    , addressOffset :: Int
    , size          :: Int
    , resetValue    :: Int
    , fields        :: [Field]
    } deriving (Show)

instance Default Register where
    def = Register "" "" "" 0 0 0 []

data Field = Field
    { name          :: Text
    , description   :: Text
    , bitOffset     :: Int
    , bitWidth      :: Int
    , access        :: Text
    } deriving (Show)

instance Default Field where
    def = Field "" "" 0 0 ""

parseSVD :: Text -> SVD
parseSVD xml = (name, ps)
    where (name, ts) = svdName $ parseTags xml
          ps = map peripheral
             $ partitions (~=="<peripheral>")
             $ dropWhile (~/="<peripheral>") ts

svdName :: [Tag Text] -> (Text, [Tag Text])
svdName ts = (innerText xs, ys)
    where (xs, ys) = break isTagClose $ dropWhile (~/="<name>") ts

peripheral :: [Tag Text] -> Peripheral
peripheral [] = def
peripheral (t:ts)
    | isTagOpenName "name" t
        = (peripheral ts) { name = ftt ts }
    | isTagOpenName "description" t
        = (peripheral ts) { description = ftt ts }
    | isTagOpenName "baseAddress" t
        = (peripheral ts) { baseAddress = fromHex $ ftt ts }
    | isTagOpenName "registers" t
        = let (us, vs) = break (~=="</registers>") ts
              rs = partitions (~=="<register>") us
          in (peripheral ts) { registers = map register rs }
    | otherwise = peripheral ts

register :: [Tag Text] -> Register
register [] = def
register (t:ts)
    | isTagOpenName "name" t
        = (register ts) { name = ftt ts }
    | isTagOpenName "displayName" t
        = (register ts) { displayName = ftt ts }
    | isTagOpenName "description" t
        = (register ts) { description = ftt ts }
    | isTagOpenName "addressOffset" t
        = (register ts) { addressOffset = fromHex $ ftt ts }
    | isTagOpenName "size" t
        = (register ts) { size = fromHex $ ftt ts }
    | isTagOpenName "resetValue" t
        = (register ts) { resetValue = fromHex $ ftt ts }
    | isTagOpenName "fields" t
        = let (us, vs) = break (~=="</fields>") ts
              rs = partitions (~=="<field>") us
          in (register ts) { fields = map field rs }
    | otherwise = register ts

field :: [Tag Text] -> Field
field [] = def
field (t:ts)
    | isTagOpenName "name" t
        = (field ts) { name = ftt ts }
    | isTagOpenName "description" t
        = (field ts) { description = ftt ts }
    | isTagOpenName "bitOffset" t
        = (field ts) { bitOffset = read $ T.unpack $ ftt ts }
    | isTagOpenName "bitWidth" t
        = (field ts) { bitWidth = read $ T.unpack $ ftt ts }
    | isTagOpenName "access" t
        = (field ts) { access = ftt ts }
    | otherwise = field ts

ftt = fromTagText . head

fromHex :: Text -> Int
fromHex t
    | ('0':'x':xs) <- s, [(n, "")] <- readHex xs = n
    | ('0':'X':xs) <- s, [(n, "")] <- readHex xs = n
    | otherwise = error $ "failed or read hex '" <> s <> "'"
    where s = T.unpack t
