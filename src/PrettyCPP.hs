{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module PrettyCPP (prettyCPP) where

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import qualified Data.Text.Lazy as TL
import Data.Aeson hiding (Options)
import Data.HashMap.Strict (fromList)
import Data.List (nub, sort, find)
import Data.List.Extra (zipWithFrom, groupSortOn)
import Data.Text as T (Text, toLower, unpack)
import Data.Maybe (fromMaybe)
import Data.Bits (shift)
import System.Directory
import System.FilePath
import Control.Monad
import Utils (writeText', hex)
import FrontEnd

prettyCPP :: FilePath -> Family -> IO ()
prettyCPP root family@Family{family=familyName,..} = do
    let dir = root </> "stm32" </> unpack (toLower familyName)
        values = familyInfo family
    forM_ (templates family) $ \(fname, template) -> do
        let fn = dir </> fname
        createDirectoryIfMissing True $ takeDirectory fn
        writeText' fn $ renderMustache template values
    let periphGroups = groupSortOn groupName $ periphTypes svd
        template = $(TH.compileMustacheFile $ "src/PrettyCPP/peripheral.h")
    forM_ periphGroups $ \periphGroup -> do
        let group = groupName $ head periphGroup
            fn = dir </> "device" </> unpack (toLower group) <.> "h"
            values = periphGroupInfo familyName group periphGroup
        writeText' fn $ renderMustache template values

familyInfo :: Family -> Value
familyInfo Family{..} = object
    [ "family"      .= family
    , "mcus"        .= (markEnds $ map (mcuInfo specs) mcus)
    , "svds"        .= (markEnds $ svdsInfo svd)
    , "ipGPIOs"     .= (markEnds $ zipWithFrom ipGPIOInfo 0 ipGPIOs)
    , "interrupt"   .= interruptInfo svd
    ]

mcuInfo :: [MCU] -> Mcu -> Value
mcuInfo mcus Mcu{..} = object
    [ "name"        .= name
    , "refName"     .= refName
    , "rpn"         .= rpn
    , "svd"         .= (\MCU{..} -> svd) mcu
    , "gpioConf"    .= ipGPIOName mcu
    ]
    where mcu = fromMaybe (error $ "can't find MCU for " <> unpack name)
              $ find (\MCU{..} -> refName == name) mcus

svdsInfo :: NormalSVD a -> [Value]
svdsInfo = map (\svd -> object [ "svd" .= svd ]) . svdNames

ipGPIOInfo :: Int -> IpGPIO -> Value
ipGPIOInfo i IpGPIO{..} = object
    [ "name"        .= name
    , "enumValue"   .= hex (shift 1 i)
    ]

periphGroupInfo :: Text -> Text -> [PeriphType] -> Value
periphGroupInfo family group xs = object
    [ "family"      .= family
    , "group"       .= group
    , "periphTypes" .= map periphTypeInfo xs
    ]

periphTypeInfo :: PeriphType -> Value
periphTypeInfo PeriphType{..} = object
    [ "typeRef"     .= periphRefInfo typeRef
    , "description" .= description
    , "registers"   .= map registerInfo registers
    , "periphInsts" .= map periphInstInfo periphInsts
    ]

periphInstInfo :: PeriphInst -> Value
periphInstInfo PeriphInst{..} = object
    [ "instRef"     .= periphRefInfo instRef
    , "baseAddress" .= hex baseAddress
    ]

periphRefInfo :: PeriphRef -> Value
periphRefInfo PeriphRef{..} = object
    [ "svd"         .= svd
    , "name"        .= name
    ]

registerInfo :: Register -> Value
registerInfo Register{..} = object
    [ "name"        .= name
    , "description" .= description
    , "resetValue"  .= hex resetValue
    , "fields"      .= map fieldInfo fields
    ]

fieldInfo :: Field -> Value
fieldInfo Field{..} = object
    [ "name"        .= name
    , "description" .= description
    , "bitOffset"   .= show bitOffset
    , "bitWidth"    .= show bitWidth
    ]

interruptInfo :: NormalSVD a -> Value
interruptInfo NormalSVD{..} = object
    [ "interrupts" .= (markEnds $ map f interrupts)
    , "registers"  .= (map g xs)
    ]
    where f Interrupt{..} = object
            [ "name"        .= name
            , "value"       .= show value
            , "description" .= description
            ]
          g i = object
            [ "suffix" .= show i
            , "lbound" .= show (i * 32)
            , "hbound" .= show ((i+1) * 32)
            ]
          xs = [0..maximum (map value interrupts) `div` 32]

markEnds :: [Value] -> [Value]
markEnds [] = []
markEnds (x:xs) = f x : xs
    where f (Object o) = Object $ o <> fromList [ "first" .= True ]

templates :: Family -> [(FilePath, Template)]
templates Family{..} =
    [ ("device/mcu.h", $(TH.compileMustacheFile $ "src/PrettyCPP/mcu.h"))
    , ("device/interrupt.h", $(TH.compileMustacheFile $ "src/PrettyCPP/interrupt.h"))
    ]
    where fam = toLower family

