{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields #-}
module FixupSVD (fixupPeripheral) where

import qualified Data.Text as T
import ParseSVD
import Utils

fixupPeripheral :: Text -> Peripheral -> Peripheral
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
fixupPeripheral _ p@Peripheral{name="CEC",..}
    = p { name = "HDMI_CEC" }
fixupPeripheral _ p@Peripheral{name="DAC",..}
    = p { name = "DAC1" }
fixupPeripheral _ p@Peripheral{name="SEC_DAC",derivedFrom=Just "DAC",..}
    = p { name = "DAC2", derivedFrom = Just "DAC1" }
fixupPeripheral _ p@Peripheral{name="LPUART",..}
    = p { name = "LPUART1" }
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

