{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module PrettyCPP (prettyFamiliesCPP, prettyFamilyCPP) where

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import qualified Data.Text.Lazy as TL
import Data.Aeson hiding (Options)
import Data.HashMap.Strict (fromList)
import Data.List (nub, sort, find)
import Data.List.Extra (zipWithFrom, groupSort, groupSortOn)
import Data.Text as T (Text, toLower, pack, unpack, intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.Bits (shift)
import System.Directory
import System.FilePath
import Control.Monad
import Utils (writeText', hex)
import FrontEnd

prettyFamiliesCPP :: FilePath -> [Text] -> IO ()
prettyFamiliesCPP root xs = do
    let fn = root </> "stm32" </> "stm32.h"
        template = $(TH.compileMustacheFile $ "src/PrettyCPP/stm32.h")
        values = object [ "families" .= (markEnds $ map nameInfo xs) ]
    createDirectoryIfMissing True $ takeDirectory fn
    writeText' fn $ renderMustache template values

prettyFamilyCPP :: FilePath -> Family -> IO ()
prettyFamilyCPP root family@Family{family=familyName,..} = do
    let dir = root </> "stm32" </> unpack (toLower familyName)
        values = familyInfo family
    forM_ (templates family) $ \(fname, template) -> do
        let fn = dir </> fname
        createDirectoryIfMissing True $ takeDirectory fn
        writeText' fn $ renderMustache template values
    let template = $(TH.compileMustacheFile $ "src/PrettyCPP/peripheral.h")
    forM_ peripherals $ \(group, (periphTypes, peripherals))  -> do
        let fn = dir </> "device" </> unpack (toLower group) <.> "h"
            values = periphInfo familyName group periphTypes peripherals
        writeText' fn $ renderMustache template values

familyInfo :: Family -> Value
familyInfo fam@Family{..} = object
    [ "family"      .= family
    , "mcus"        .= (markEnds $ map (mcuInfo specs) mcus)
    , "svds"        .= (markEnds $ map nameInfo svds)
    , "configs"     .= (markEnds $ map nameValInfo configs)
    , "periphs"     .= (markEnds $ map nameInfo $ peripheralNames fam)
    , "periphInsts" .= (map periphInstInfo $ peripheralInsts fam)
    , "allGroups"   .= (map nameInfo $ map fst peripherals)
    , "interrupt"   .= interruptInfo interrupts
    , "gpio"        .= gpioInfo gpio
    ]
    where GPIO{..} = gpio

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

gpioInfo :: GPIO -> Value
gpioInfo GPIO{..} = object
    [ "ports"   .= (markEnds $ map nameValInfo ports)
    , "pins"    .= (markEnds $ map nameValInfo pins)
    , "signals" .= (markEnds $ map nameInfo $ signalNames signals)
    , "altfuns" .= (markEnds $ map nameValInfo $ altfunNames signals)
    , "traits"  .= map pinSignalAF signals
    ]
    where pinSignalAF :: Signal -> Value
          pinSignalAF sig@Signal{..} = object $
              [ "pin"       .= pin
              , "signal"    .= signal
              ] ++ lhs ++ rhs
              where lhs | length confs < n
                        = [ "condLHS" .= True
                          , "config" .= orExpr confs
                          ]
                        | otherwise = []
                        where confs = map snd altfun
                    rhs | ((af, _):[]) <- xs
                        = [ "simpleRHS" .= True
                          , "altfun" .= af
                          ]
                        | ((af1, c1s):(af2, c2s):[]) <- xs
                        = ("dualRHS" .= True) : if length c1s < length c2s
                          then
                            [ "cond" .= orExpr c1s
                            , "altfun1" .= af1
                            , "altfun2" .= af2
                            ]
                          else
                            [ "cond" .= orExpr c2s
                            , "altfun1" .= af2
                            , "altfun2" .= af1
                            ]
                        | otherwise = error $ "too complex: " <> show sig
                        where xs = groupSort altfun
          n = length configs
          orExpr (x:[]) = x
          orExpr xs = "(" <> intercalate "|" xs <> ")"

periphInfo :: Text -> Text -> [PeriphType Reserve] -> [Peripheral] -> Value
periphInfo family groupName xs ys = object
    [ "family"      .= family
    , "group"       .= groupName
    , "groupLC"     .= toLower groupName
    , "periphTypes" .= map periphTypeInfo xs
    , "peripherals" .= map peripheralInfo ys
    ]

periphTypeInfo :: PeriphType Reserve -> Value
periphTypeInfo PeriphType{..} = object
    [ "typeRef"     .= periphRefInfo typeRef
    , "description" .= description
    , "registers"   .= map registerInfo registers
    , "periphInsts" .= map periphInstInfo periphInsts
    ]

peripheralInfo :: Peripheral -> Value
peripheralInfo Peripheral{..} = object $
    [ "name"        .= name
    , "nameLC"      .= toLower name
    , "altFuns"     .= map (\x -> object [ "altFun" .= x ]) altFuns
    , "haveTraits"  .= (isJust control || not (null altFuns))
    ] ++
    [ "instNo"      .= pack (show no) | Just no <- [ instNo ] ] ++
    [ "controls"    .= controlInfo c | Just c <- [ control ] ]

controlInfo :: ClockControl -> [Value]
controlInfo ClockControl{..} = concat
    [ [ f "enable" rf True | Just rf <- [ enable ] ]
    , [ f "disable" rf False | Just rf <- [ enable ] ]
    , [ f "enable_sleep_mode" rf True | Just rf <- [ enableSM ] ]
    , [ f "disable_sleep_mode" rf False | Just rf <- [ enableSM ] ]
    , [ f "reset" rf True | Just rf <- [ reset ] ]
    ]
    where f :: Text -> (Text, Text) -> Bool -> Value
          f method (register, flag) en = object
              [ "method"    .= method
              , "register"  .= register
              , "flag"      .= flag
              , "en"        .= en
              ]

periphInstInfo :: PeriphInst -> Value
periphInstInfo PeriphInst{..} = object
    [ "instRef"     .= periphRefInfo instRef
    , "baseAddress" .= hex baseAddress
    ]

periphRefInfo :: PeriphRef -> Value
periphRefInfo PeriphRef{..} = object
    [ "svd"         .= svd
    , "svdLC"       .= toLower svd
    , "name"        .= name
    , "nameLC"      .= toLower name
    ]

registerInfo :: Either Reserve Register -> Value
registerInfo (Right Register{..}) = object
    [ "name"        .= name
    , "type"        .= case size of
        16  -> pack "volatile uint16_t"
        32  -> pack "volatile uint32_t"
        n   -> error $ "unsupported register size: " <> show n
    , "description" .= description
    , "fields"      .= (resetField : map (fieldInfo name) fields)
    ]
    where resetField = object
            [ "name"        .= (name <> "_RESET_VALUE")
            , "type"        .= pack "static constexpr uint32_t"
            , "value"       .= hex resetValue
            , "description" .= pack "Reset value"
            ]

registerInfo (Left Reserve{..}) = object
    [ "name"        .= ("_" <> hex addressOffset)
    , "size"        .= hex (size `div` 32)
    , "reserve"     .= True
    ]

fieldInfo :: Text -> Field -> Value
fieldInfo regName Field{..}
  | bitWidth == 1 = object
    [ "name"        .= (regName <> "_" <> name)
    , "type"        .= pack "static constexpr uint32_t"
    , "value"       .= hex (shift 1 bitOffset)
    , "description" .= description
    ]
  | otherwise = object
    [ "name"        .= (regName <> "_" <> name)
    , "pos"         .= pos
    , "mask"        .= mask
    , "description" .= description
    ]
    where pos = pack $ show bitOffset
          mask = hex $ shift 0xffffffff (bitWidth - 32)

interruptInfo :: [Interrupt] -> Value
interruptInfo interrupts = object
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

nameInfo :: Text -> Value
nameInfo n = object [ "name" .= n, "nameLC" .= toLower n ]

nameValInfo :: (Text, Int) -> Value
nameValInfo (n, v) = object [ "name" .= n, "value" .= hex v ]

templates :: Family -> [(FilePath, Template)]
templates Family{..} =
    [ ("device/mcu.h", $(TH.compileMustacheFile $ "src/PrettyCPP/mcu.h"))
    , ("device/mcu.cpp", $(TH.compileMustacheFile $ "src/PrettyCPP/mcu.cpp"))
    , ("device/all.h", $(TH.compileMustacheFile $ "src/PrettyCPP/all.h"))
    , ("device/pins.h", $(TH.compileMustacheFile $ "src/PrettyCPP/pins.h"))
    , ("device/interrupt.h", $(TH.compileMustacheFile $ "src/PrettyCPP/interrupt.h"))
    , ("device/vector.h", $(TH.compileMustacheFile $ "src/PrettyCPP/vector.h"))
    ]
    where fam = toLower family

