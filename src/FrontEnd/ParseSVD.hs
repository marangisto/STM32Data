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
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.List (sortOn)
import Utils (fromHex, cleanWords, packUpper, packWords)
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
    } deriving (Show)

data Interrupt = Interrupt
    { name          :: !Text
    , description   :: !Text
    , value         :: !Int
    } deriving (Show)

data Register = Register
    { name          :: !Text
    , displayName   :: !Text
    , description   :: !Text
    , addressOffset :: !Int
    , size          :: !Int
    , access        :: !(Maybe Text)
    , resetValue    :: !Int
    , fields        :: ![Field]
    } deriving (Show)

data Field = Field
    { name          :: !Text
    , description   :: !Text
    , bitOffset     :: !Int
    , bitWidth      :: !Int
    } deriving (Show)

parseSVD :: FilePath -> IO SVD
parseSVD fn = do
    s <- readFile fn
    xs <- runX (readString [ withValidate yes ] s >>> getSVD)
    case xs of
        [x] -> return x
        _ -> error "failed to parse SVD"

getSVD = atTag "device" >>> proc x -> do
    name <- arr pack <<< elemText "name" -< x
    version <- arr pack <<< elemText "version" -< x
    description <- arr packWords <<< elemText "description" -< x
    peripherals <- listA getPeripheral <<< list "peripherals" -< x
    interrupts <- listA getInterrupt -< x
    returnA -< SVD{..}

getPeripheral = atTag "peripheral" >>> proc x -> do
    derivedFrom <- arr (fmap packUpper) <<< attrTextMay "derivedFrom" -< x
    name <- arr packUpper <<< elemText "name" -< x
    description <- arr (maybe "" packWords) <<< elemTextMay "description" -< x
    groupName <- arr (maybe "" packUpper) <<< elemTextMay "groupName" -< x
    baseAddress <- arr (fromHex . pack) <<< elemText "baseAddress" -< x
    registers <- ( listA getRegister <<< list "registers"
                 ) `orElse` constA [] -< x
    returnA -< Peripheral{..}

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

getField = atTag "field" >>> proc x -> do
    name <- arr packUpper <<< elemText "name" -< x
    description <- arr packWords <<< elemText "description" -< x
    bitOffset <- arr read <<< elemText "bitOffset" -< x
    bitWidth <- arr read <<< elemText "bitWidth" -< x
    returnA -< Field{..}

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

