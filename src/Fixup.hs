{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Fixup (fixup) where

import Normalize
import Utils
import qualified Data.Text as T

fixup :: NormalSVD -> NormalSVD
fixup x@NormalSVD{..} = x { periphTypes = ps }
    where ps = map (editPeriphType family) periphTypes

editPeriphType :: Text -> PeriphType -> PeriphType
editPeriphType family p@PeriphType{..} = p { registers = registers' }
    where registers' = map (editRegister family periphName) registers
          PeriphRef{name=periphName} = typeRef

editRegister :: Text -> Text -> Register -> Register
editRegister f p r@Register{..} = r { fields = fields' }
    where fields' = map (editField f p name) fields

editField :: Text -> Text -> Text -> Field -> Field
editField = runFieldEdits fieldEdits

type FieldEdit = Text -> Text -> Text -> Field -> Field

runFieldEdits :: [FieldEdit] -> FieldEdit
runFieldEdits fs f p r = foldl (.) id (map (($r) . ($p) . ($f)) fs)

fieldEdits :: [FieldEdit]
fieldEdits =
    [ compx_csr
    , rcc_fields
    ]

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
fixupPeriphType _ p@PeriphType{..}
    | name typeRef `elem` [ "DAC", "LPUART", "SAI", "QUADSPI" ]
    = p { name = name <> "1" }

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
fixupPeriphType _ p@PeriphType{name="USB_FS_DEVICE",..}
    = p { name = "USB" }
fixupPeriphType _ p@PeriphType{name="CEC",..}
    = p { name = "HDMI_CEC" }
fixupPeriphType _ p@PeriphType{name="SEC_DAC",derivedFrom=Just "DAC",..}
    = p { name = "DAC2", derivedFrom = Just "DAC1" }
fixupPeriphType _ p@PeriphType{name="LPTIMER1",..}
    = p { name = "LPTIM1" }
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
