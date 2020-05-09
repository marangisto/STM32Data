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

