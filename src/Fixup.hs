{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Fixup (fixup) where

import Normalize
import Utils
import qualified Data.Text as T
import Data.List (sortOn)

fixup :: NormalSVD -> NormalSVD
fixup x@NormalSVD{..} = x { periphTypes = ps }
    where ps = map (editPeriphType family) periphTypes

editPeriphType :: Text -> PeriphType -> PeriphType
editPeriphType f p@PeriphType{..}
    = runPeriphTypeEdits periphTypeEdits f
    $ p { registers = map (editRegister f name) registers
        , periphInsts = map (editPeriphInst f name) periphInsts
        }
    where PeriphRef{..} = typeRef

editPeriphInst :: Text -> Text -> PeriphInst -> PeriphInst
editPeriphInst = runPeriphInstEdits periphInstEdits

editRegister :: Text -> Text -> Register -> Register
editRegister f p r@Register{..}
    = runRegisterEdits registerEdits f p
    $ r { fields = map (editField f p name) fields }

editField :: Text -> Text -> Text -> Field -> Field
editField = runFieldEdits fieldEdits

type PeriphTypeEdit = Text -> PeriphType -> PeriphType
type PeriphInstEdit = Text -> Text -> PeriphInst -> PeriphInst
type RegisterEdit = Text -> Text -> Register -> Register
type FieldEdit = Text -> Text -> Text -> Field -> Field

runPeriphTypeEdits :: [PeriphTypeEdit] -> PeriphTypeEdit
runPeriphTypeEdits xs f = foldl (.) id (map ($f) xs)

runPeriphInstEdits :: [PeriphInstEdit] -> PeriphInstEdit
runPeriphInstEdits xs f p = foldl (.) id (map (($p) . ($f)) xs)

runRegisterEdits :: [RegisterEdit] -> RegisterEdit
runRegisterEdits xs f p = foldl (.) id (map (($p) . ($f)) xs)

runFieldEdits :: [FieldEdit] -> FieldEdit
runFieldEdits xs f p r = foldl (.) id (map (($r) . ($p) . ($f)) xs)

periphTypeEdits :: [PeriphTypeEdit]
periphTypeEdits =
    [
    ]

periphInstEdits :: [PeriphInstEdit]
periphInstEdits =
    [ inst_name
    ]

registerEdits :: [RegisterEdit]
registerEdits =
    [ gpio_fields
    ]

fieldEdits :: [FieldEdit]
fieldEdits =
    [ compx_csr
    , rcc_fields
    ]

inst_name :: PeriphInstEdit
inst_name _ _ x@PeriphInst{instRef=r@PeriphRef{..}}
    | name == "LPTIMER1" = x { instRef = r { name = "LPTIM1" } }
    | name == "CEC" = x { instRef = r { name = "HDMI_CEC" } }
    | name == "USB_FS_DEVICE" = x { instRef = r { name = "USB" } }
    | name `elem` [ "DAC", "LPUART", "SAI", "QUADSPI" ]
        = x { instRef = r { name = name <> "1" } }
    | otherwise = x

gpio_fields :: RegisterEdit
gpio_fields "STM32F0" "RCC" x@Register{fields=fs,..}
    | name == "AHBENR" = x { fields = sortOn bitOffset $ en : fs }
    | name == "AHBRSTR" = x { fields = sortOn bitOffset $ rst : fs }
    | otherwise = x
    where en = Field "IOPEEN" "I/O port E clock enable" 21 1
          rst = Field "IOPERST" "I/O port E reset" 21 1
gpio_fields _ _ x = x

compx_csr :: FieldEdit
compx_csr _ p r x@Field{..}
    | p == "SYSCFG_COMP_OPAMP", "COMP" `T.isPrefixOf` r
    , "_CSR" `T.isSuffixOf` r, "INMSEL" `T.isSuffixOf` name
    , bitOffset == 22 = x { name = name <> "3" }
    | otherwise = x

rcc_fields :: FieldEdit
rcc_fields _ "RCC" _ x@Field{..}
    | name == "AD12CSMEN" = x { name = "ADC12_COMMONSMEN" }
    | name == "ADC12EN" = x { name = "ADC12_COMMONEN" }
    | name == "ADC12SMEN" = x { name = "ADC12_COMMONSMEN" }
    | name == "ADC12RST" = x { name = "ADC12_COMMONRST" }
    | name == "ADC345EN" = x { name = "ADC345_COMMONEN" }
    | name == "ADC345SMEN" = x { name = "ADC345_COMMONSMEN" }
    | name == "ADC345RST" = x { name = "ADC345_COMMONRST" }
    | name == "DMAMUX1EN" = x { name = "DMAMUXEN" }
    | name == "DMAMUX1SMEN" = x { name = "DMAMUXSMEN" }
    | name == "DMAMUX1RST" = x { name = "DMAMUXRST" }
    | name == "CRYPTEN" = x { name = "AESEN" }
    | name == "CRYPTSMEN" = x { name = "AESSMEN" }
    | name == "CRYPTRST" = x { name = "AESRST" }
    | name == "HRTIMEREN" = x { name = "HRTIM_MASTEREN" }
    | name == "HRTIMERSMEN" = x { name = "HRTIM_MASTERSMEN" }
    | name == "HRTIM1RST" = x { name = "HRTIM_MASTERRST" }
    | name == "SP3EN" = x { name = "SPI3EN" }
    | name == "SP3SMEN" = x { name = "SPI3SMEN" }
    | name == "SP3RST" = x { name = "SPI3RST" }
    | name == "USBDEN" = x { name = "USBEN" }
    | name == "USBDSMEN" = x { name = "USBSMEN" }
    | name == "USBDRST" = x { name = "USBRST" }
    | name == "USBPDEN" = x { name = "UCPD1EN" }
    | name == "USBPDSMEN" = x { name = "UCPD1SMEN" }
    | name == "USBPDRST" = x { name = "UCPD1RST" }
    | name == "FLITFRST" = x { name = "FLASHRST" }
    | name == "FLITFEN" = x { name = "FLASHEN" }
    | name == "FLITFSMEN" = x { name = "FLASHSMEN" }
    | name == "ADC345RST_" = x { name = "ADC345_COMMONRST" }
    | name == "DAC1RST_" = x { name = "DAC1RST" }
    | name == "FLITFRST_" = x { name = "FLASHRST" }
    | otherwise = x
rcc_fields _ _ _ x = x

{-
fixupPeriphType "STM32G0" p@PeriphType{..}
    | name == "ADC"
    = p { registers = map adc_hwcfgr6 registers }
fixupPeriphType "STM32L4" p@PeriphType{..}
    | name == "USART1"
    = p { registers = map usart1_cr1 registers }
fixupPeriphType _ p@PeriphType{name="USB",..}
    = p { registers = filter (not . usb_buffer) registers }
fixupPeriphType _ p@PeriphType{name="NVIC",..}
    = p { registers = map nvic_iserx registers }
fixupPeriphType _ p@PeriphType{name="SEC_DAC",derivedFrom=Just "DAC",..}
    = p { name = "DAC2", derivedFrom = Just "DAC1" }
fixupPeriphType _ p@PeriphType{name="STK",..}
    = p { registers = map stk_regs registers }
fixupPeriphType _ p = p

adc_hwcfgr6 :: Register -> Register
adc_hwcfgr6 r@Register{..}
    | name == "HWCFGR6"
    , resetValue == fromHex "0x1f1f1f11f"
    = r { resetValue = fromHex "0x1f1f1f1f" }
    | otherwise = r

usart1_cr1 :: Register -> Register
usart1_cr1 r@Register{..}
    | name == "0x00000000"
    = r { name = "CR1", displayName = "CR1" }
    | otherwise = r

usb_buffer :: Register -> Bool
usb_buffer Register{..}
    | "ADDR" `T.isPrefixOf` name = True
    | "COUNT" `T.isPrefixOf` name = True
    | otherwise = False

nvic_iserx :: Register -> Register
nvic_iserx r@Register{..}
    | name `elem` [ "ISER", "ICER", "ISPR", "ICPR" ]
    = r { name = name <> "0" }
    | otherwise = r

stk_regs :: Register -> Register
stk_regs r@Register{..}
    | name == "CTRL" = r { name = "CSR" }
    | name == "LOAD" = r { name = "RVR" }
    | name == "VAL" = r { name = "CVR" }
    | otherwise = r

-}

