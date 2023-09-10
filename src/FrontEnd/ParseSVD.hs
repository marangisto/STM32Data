{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FrontEnd.ParseSVD
    ( SVD(..)
    , Peripheral(..)
    , Interrupt(..)
    , Register(..)
    , Field(..)
    , parseSVD
    ) where

import Data.Hashable
import Data.Text (Text, pack)
import Data.List (sortOn)
import Utils (fromHex, packUpper, packWords)
import HXT

data SVD = SVD
    { name          :: !Text
    , version       :: !Text
    , description   :: !Text
    , peripherals   :: ![Peripheral]
    , interrupts    :: ![Interrupt]
    } deriving (Show)

data Peripheral = Peripheral
    { name          :: !Text
    , description   :: !Text
    , groupName     :: !Text
    , baseAddress   :: !Int
    , registers     :: ![Register]
    , derivedFrom   :: !(Maybe Text)
    } deriving (Eq, Show)

data Interrupt = Interrupt
    { name          :: !Text
    , description   :: !Text
    , value         :: !Int
    } deriving (Eq, Show)

data Register = Register
    { name          :: !Text
    , displayName   :: !Text
    , description   :: !Text
    , addressOffset :: !Int
    , size          :: !Int
    , access        :: !(Maybe Text)
    , resetValue    :: !Int
    , fields        :: ![Field]
    } deriving (Eq, Show)

data Field = Field
    { name          :: !Text
    , description   :: !Text
    , bitOffset     :: !Int
    , bitWidth      :: !Int
    } deriving (Eq, Show)

parseSVD :: FilePath -> IO SVD
parseSVD fn = do
    s <- readFile fn
    xs <- runX (readString [ withValidate yes ] s >>> getSVD)
    case xs of
        [x] -> return x
        _ -> error "failed to parse SVD"

getSVD :: ArrowXml cat => cat (NTree XNode) SVD
getSVD = atTag "device" >>> proc x -> do
    name <- arr pack <<< elemText "name" -< x
    version <- arr pack <<< elemText "version" -< x
    description <- arr packWords <<< elemText "description" -< x
    peripherals <- listA getPeripheral <<< list "peripherals" -< x
    interrupts <- listA getInterrupt -< x
    returnA -< SVD{..}

getPeripheral :: ArrowXml cat => cat (NTree XNode) Peripheral
getPeripheral = atTag "peripheral" >>> proc x -> do
    derivedFrom <- arr (fmap packUpper) <<< attrTextMay "derivedFrom" -< x
    name <- arr packUpper <<< elemText "name" -< x
    description <- arr (maybe "" packWords) <<< elemTextMay "description" -< x
    groupName <- arr (maybe "" packUpper) <<< elemTextMay "groupName" -< x
    baseAddress <- arr (fromHex . pack) <<< elemText "baseAddress" -< x
    registers <- ( listA getRegister <<< list "registers"
                 ) `orElse` constA [] -< x
    returnA -< Peripheral{..}

getRegister :: ArrowXml cat => cat (NTree XNode) Register
getRegister = atTag "register" >>> proc x -> do
    name <- arr packUpper <<< elemText "name" -< x
    displayName <- arr pack <<< elemText "displayName" -< x
    description <- arr packWords <<< elemText "description" -< x
    addressOffset <- arr (fromHex . pack) <<< elemText "addressOffset" -< x
    size <- arr (read) <<< elemText "size" -< x
    access <- arr (fmap pack) <<< elemTextMay "access" -< x
    resetValue <- arr (fromHex . pack) <<< elemText "resetValue" -< x
    fields <- listA getField <<< list "fields" -< x
    returnA -< Register{..}

getField :: ArrowXml cat => cat (NTree XNode) Field
getField = atTag "field" >>> proc x -> do
    name <- arr packUpper <<< elemText "name" -< x
    description <- arr packWords <<< elemText "description" -< x
    bitOffset <- arr read <<< elemText "bitOffset" -< x
    bitWidth <- arr read <<< elemText "bitWidth" -< x
    returnA -< Field{..}

getInterrupt :: ArrowXml cat => cat (NTree XNode) Interrupt
getInterrupt = atTag "interrupt" >>> proc x -> do
    name <- arr packUpper <<< elemText "name" -< x
    description <- arr packWords <<< elemText "description" -< x
    value <- arr read <<< elemText "value" -< x
    returnA -< Interrupt{..}

instance Hashable Peripheral where
    hashWithSalt h Peripheral{..} = hashWithSalt h
        ( sortOn addressOffset registers
        , derivedFrom
        )

instance Hashable Interrupt where
    hashWithSalt h Interrupt{..} = hashWithSalt h
        ( name
        , value
        )

instance Hashable Register where
    hashWithSalt h Register{..} = hashWithSalt h
        ( name
        , addressOffset
        , resetValue
        , sortOn bitOffset fields
        )

instance Hashable Field where
    hashWithSalt h Field{..} = hashWithSalt h
        ( name
        , bitOffset
        , bitWidth
        )

