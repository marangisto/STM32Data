{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module ParseSVD
    ( SVD(..)
    , Peripheral(..)
    , Interrupt(..)
    , Register(..)
    , Field(..)
    , parseSVD
    ) where

import Data.List (break)
import qualified Data.Text as T
import Numeric (readHex)
import Data.Default
import TagSoup

instance Default T.Text where def = ""

data SVD = SVD
    { name          :: Text
    , description   :: Text
    , version       :: Text
    , peripherals   :: [Peripheral]
    } deriving (Show)

instance Default SVD where
    def = SVD def def def def

data Peripheral = Peripheral
    { name          :: Text
    , description   :: Text
    , baseAddress   :: Int
    , interrupts    :: [Interrupt]
    , registers     :: [Register]
    , derivedFrom   :: Maybe Text
    } deriving (Show)

instance Default Peripheral where
    def = Peripheral def def def def def def

data Interrupt = Interrupt
    { name          :: Text
    , description   :: Text
    , value         :: Int
    } deriving (Show)

instance Default Interrupt where
    def = Interrupt def def def

data Register = Register
    { name          :: Text
    , displayName   :: Text
    , description   :: Text
    , addressOffset :: Int
    , size          :: Int
    , access        :: Maybe Text
    , resetValue    :: Int
    , fields        :: [Field]
    } deriving (Show)

instance Default Register where
    def = Register def def def def def def def def

data Field = Field
    { name          :: Text
    , description   :: Text
    , bitOffset     :: Int
    , bitWidth      :: Int
    } deriving (Show)

instance Default Field where
    def = Field def def def def

parseSVD :: Text -> SVD
parseSVD = svd . parseTags

svd :: [Tag Text] -> SVD
svd [] = def
svd (t:ts)
    | isTagOpenName "name" t
        = (svd ts) { name = ftt ts }
    | isTagOpenName "version" t
        = (svd ts) { version = ftt ts }
    | isTagOpenName "description" t
        = (svd ts) { description = ftt ts }
    | isTagOpenName "peripherals" t
        = let (us, rest) = break (~=="</peripherals>") ts
              ps = partitions (~=="<peripheral>") us
          in (svd rest) { peripherals = map peripheral ps }
    | otherwise = svd ts

peripheral :: [Tag Text] -> Peripheral
peripheral [] = def
peripheral (t:ts)
    | isTagOpenName "peripheral" t
        = let s = fromAttrib "derivedFrom" t
              d = if s /= "" then Just s else Nothing
           in (peripheral ts) { derivedFrom = d }
    | isTagOpenName "name" t
        = (peripheral ts) { name = ftt ts }
    | isTagOpenName "description" t
        = (peripheral ts) { description = ftt ts }
    | isTagOpenName "baseAddress" t
        = (peripheral ts) { baseAddress = fromHex $ ftt ts }
    | isTagOpenName "interrupt" t
        = let (us, rest) = break (~=="</interrupt>") ts
              p = peripheral rest
          in p { interrupts = interrupt us : interrupts p }
    | isTagOpenName "registers" t
        = let (us, rest) = break (~=="</registers>") ts
              rs = partitions (~=="<register>") us
          in (peripheral rest) { registers = map register rs }
    | otherwise = peripheral ts

interrupt :: [Tag Text] -> Interrupt
interrupt [] = def
interrupt (t:ts)
    | isTagOpenName "name" t
        = (interrupt ts) { name = ftt ts }
    | isTagOpenName "description" t
        = (interrupt ts) { description = ftt ts }
    | isTagOpenName "bitOffset" t
        = (interrupt ts) { value = read $ T.unpack $ ftt ts }
    | otherwise = interrupt ts

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
    | isTagOpenName "access" t
        = (register ts) { access = Just $ ftt ts }
    | isTagOpenName "resetValue" t
        = (register ts) { resetValue = fromHex $ ftt ts }
    | isTagOpenName "fields" t
        = let (us, rest) = break (~=="</fields>") ts
              rs = partitions (~=="<field>") us
          in (register rest) { fields = map field rs }
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
    | otherwise = field ts

ftt = fromTagText . head

fromHex :: Text -> Int
fromHex t
    | ('0':'x':xs) <- s, [(n, "")] <- readHex xs = n
    | ('0':'X':xs) <- s, [(n, "")] <- readHex xs = n
    | [(n, "")] <- readHex s = n
    | otherwise = error $ "failed or read hex '" <> s <> "'"
    where s = T.unpack t

