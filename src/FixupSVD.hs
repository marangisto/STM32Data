{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields #-}
module FixupSVD (fixupPeripheral) where

import qualified Data.Text as T
import ParseSVD
import Utils

fixupPeripheral :: Text -> Peripheral -> Peripheral
fixupPeripheral _ p@Peripheral{..}
    | name `elem` [ "DAC", "LPUART", "SAI", "QUADSPI" ]
    = p { name = name <> "1" }
fixupPeripheral "STM32F3" p@Peripheral{..}
    | name == "SYSCFG_COMP_OPAMP"
    = p { registers = map compx_csr registers }
fixupPeripheral "STM32G0" p@Peripheral{..}
    | name == "ADC"
    = p { registers = map adc_hwcfgr6 registers }
fixupPeripheral "STM32L4" p@Peripheral{..}
    | name == "USART1"
    = p { registers = map usart1_cr1 registers }
fixupPeripheral _ p@Peripheral{name="USB",..}
    = p { registers = filter (not . usb_buffer) registers }
fixupPeripheral _ p@Peripheral{name="NVIC",..}
    = p { registers = map nvic_iserx registers }
fixupPeripheral _ p@Peripheral{name="USB_FS_DEVICE",..}
    = p { name = "USB" }
fixupPeripheral _ p@Peripheral{name="CEC",..}
    = p { name = "HDMI_CEC" }
fixupPeripheral _ p@Peripheral{name="SEC_DAC",derivedFrom=Just "DAC",..}
    = p { name = "DAC2", derivedFrom = Just "DAC1" }
fixupPeripheral _ p@Peripheral{name="LPTIMER1",..}
    = p { name = "LPTIM1" }
fixupPeripheral "STM32G4" p@Peripheral{name="RCC",..}
    = p { registers = map rcc_adc registers }
fixupPeripheral _ p = p

compx_csr :: Register -> Register
compx_csr r@Register{..}
    | "COMP" `T.isPrefixOf` name 
    , "_CSR" `T.isSuffixOf` name
    = r { fields = map inmsel fields }
    | otherwise = r
    where inmsel :: Field -> Field
          inmsel f@Field{..}
            | "INMSEL" `T.isSuffixOf` name
            , bitOffset == 22
            = f { name = name <> "3" }
            | otherwise = f

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

rcc_adc :: Register -> Register
rcc_adc r@Register{..} = r { fields = map f fields }
    where f :: Field -> Field
          f x@Field{..}
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


