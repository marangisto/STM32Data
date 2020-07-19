{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module FrontEnd.Fixup (Reserve(..), fixup) where

import FrontEnd.Normalize
import Utils
import Data.Text (isPrefixOf, isSuffixOf, toUpper)
import Data.Text (stripPrefix, stripSuffix, dropWhileEnd)
import Data.List.Extra (nubOrdOn, groupSortOn)
import Data.List (sortOn, mapAccumL)
import Data.Char (isDigit)

data Reserve = Reserve
    { name          :: !Text
    , addressOffset :: !Int
    , size          :: !Int
    } deriving (Show)

fixup :: NormalSVD () Void -> NormalSVD () Reserve
fixup x@NormalSVD{..} = x
    { periphTypes = ps
    , interrupts = exceptions ++ interrupts
    }
    where ps = map (f . editPeriphType family) periphTypes
          f p@PeriphType{..} = p {registers = reserve registers }

editPeriphType :: Text -> PeriphType Void -> PeriphType Void
editPeriphType f p@PeriphType{..}
    = runPeriphTypeEdits periphTypeEdits f
    $ p { registers = map (fmap $ editRegister f name) registers
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

type PeriphTypeEdit = Text -> PeriphType Void -> PeriphType Void
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
    [ usb_regs
    , group_name
    , sys_tick
    ]

periphInstEdits :: [PeriphInstEdit]
periphInstEdits =
    [ inst_name
    ]

registerEdits :: [RegisterEdit]
registerEdits =
    [ reg_name
    , gpio_fields
    , tim_ccmr
    , adc_hwcfgr6
    , nvic_iserx
    , syscfg_prefix
    , rcc_pllcfgr
    , gpio_reg
    , usart_cr1
    ]

fieldEdits :: [FieldEdit]
fieldEdits =
    [ compx_csr
    , rcc_fields
    ]

usb_regs :: PeriphTypeEdit
usb_regs _ x@PeriphType{typeRef=PeriphRef{..},..}
    | name == "USB" = x { registers = filter (not . usb_buffer) registers }
    | otherwise = x

group_name :: PeriphTypeEdit
group_name _ x@PeriphType{typeRef=PeriphRef{..},..}
    | groupName == "USART", "LPUART" `isPrefixOf` name
    = x { groupName = "LPUART" }
    | groupName == "USART", "UART" `isPrefixOf` name
    = x { groupName = "UART" }
    | groupName == "FSMC", name == "FMC"
    = x { groupName = "FMC" }
    | otherwise = x

sys_tick :: PeriphTypeEdit
sys_tick _ x@PeriphType{typeRef=PeriphRef{..},..}
    | name == "STK" = x { registers = map (fmap f) registers }
    | otherwise = x
    where f :: Register -> Register
          f r@Register{..}
            | name == "CTRL" = r { name = "CSR" }
            | name == "LOAD" = r { name = "RVR" }
            | name == "LOAD_" = r { name = "RVR" }
            | name == "VAL" = r { name = "CVR" }
            | otherwise = r

inst_name :: PeriphInstEdit
inst_name fam _ x@PeriphInst{instRef=r@PeriphRef{..}}
    | name == "PF" = x { instRef = r { name = "PF_" } }
    | name == "LPTIMER1" = x { instRef = r { name = "LPTIM1" } }
    | name == "CEC" = x { instRef = r { name = "HDMI_CEC" } }
    | name == "USB_FS_DEVICE" = x { instRef = r { name = "USB" } }
    | name `elem` [ "DAC", "LPUART", "SAI", "QUADSPI" ]
        = x { instRef = r { name = name <> "1" } }
    | fam == "STM32L1", Just s <- stripPrefix "UART" name
        = x { instRef = r { name = "USART" <> s } }
    | otherwise = x

reg_name :: RegisterEdit
reg_name "STM32F7" "RCC" x@Register{name="DKCFGR1"}
    = x { name = "DCKCFGR1" }
reg_name _ _ x = x

gpio_fields :: RegisterEdit
gpio_fields "STM32F0" "RCC" x@Register{fields=fs,..}
    | name == "AHBENR" = x { fields = sortOn bitOffset $ en : fs }
    | name == "AHBRSTR" = x { fields = sortOn bitOffset $ rst : fs }
    | otherwise = x
    where en = Field "IOPEEN" "I/O port E clock enable" 21 1
          rst = Field "IOPERST" "I/O port E reset" 21 1
gpio_fields _ _ x = x

tim_ccmr :: RegisterEdit
tim_ccmr _ p x@Register{..}
    | tim, Just s <- stripSuffix "_INPUT" name = x { name = s }
    | tim, Just s <- stripSuffix "_OUTPUT" name = x { name = s }
    | otherwise = x
    where tim = "TIM" `isPrefixOf` p

adc_hwcfgr6 :: RegisterEdit
adc_hwcfgr6 _ p x@Register{..}
    | p == "ADC", name == "HWCFGR6"
    , resetValue == fromHex "0x1f1f1f11f"
    = x { resetValue = fromHex "0x1f1f1f1f" }
    | otherwise = x

nvic_iserx :: RegisterEdit
nvic_iserx _ p x@Register{..}
    | p == "NVIC", name `elem` [ "ISER", "ICER", "ISPR", "ICPR" ]
    = x { name = name <> "0" }
    | otherwise = x

syscfg_prefix :: RegisterEdit
syscfg_prefix "STM32F0" "SYSCFG" x@Register{..}
    | Just rest <- stripPrefix "SYSCFG_" name
    = x { name = rest }
    | otherwise = x
syscfg_prefix _ _ x = x

rcc_pllcfgr :: RegisterEdit
rcc_pllcfgr family "RCC" x@Register{..}
    | family `elem` [ "STM32F4", "STM32F7" ]
    , name `elem` [ "PLLCFGR", "CFGR" ] = x { fields = f fields }
    | otherwise = x
    where f :: [Field] -> [Field]
          f = sortOn bitOffset . map h
            . groupSortOn (\Field{..} -> name) . map g
          g :: Field -> Field
          g x@Field{..}
              | any (`isPrefixOf` name) [ "PLL", "SW" ]
              = x { name = dropWhileEnd isDigit name }
              | otherwise = x
          h :: [Field] -> Field
          h [] = error "impossible!"
          h (x:[]) = x
          h xs@(x:_) = x
              { bitOffset = minimum $ map (\Field{..} -> bitOffset) xs
              , bitWidth = length xs
              }
rcc_pllcfgr _ _ x = x

gpio_reg :: RegisterEdit
gpio_reg "STM32F7" p x@Register{..}
    | "GPIO" `isPrefixOf` p, name == "GPIOB_OSPEEDR"
    = x { name = "OSPEEDR" }
    | otherwise = x
gpio_reg _ _ x = x

usart_cr1 :: RegisterEdit
usart_cr1 "STM32L4" p x@Register{..}
    | "USART" `isPrefixOf` p, name == "0X00000000"
    = x { name = "CR1", displayName = "CR1" }
    | otherwise = x
usart_cr1 _ _ x = x

compx_csr :: FieldEdit
compx_csr _ p r x@Field{..}
    | p == "SYSCFG_COMP_OPAMP", "COMP" `isPrefixOf` r
    , "_CSR" `isSuffixOf` r, "INMSEL" `isSuffixOf` name
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

usb_buffer :: Either Void Register -> Bool
usb_buffer (Right Register{..})
    | "ADDR" `isPrefixOf` name = True
    | "COUNT" `isPrefixOf` name = True
    | otherwise = False
usb_buffer _ = False

{-
fixupPeriphType "STM32L4" p@PeriphType{..}
    | name == "USART1"
    = p { registers = map usart1_cr1 registers }
fixupPeriphType _ p@PeriphType{name="SEC_DAC",derivedFrom=Just "DAC",..}
    = p { name = "DAC2", derivedFrom = Just "DAC1" }
-}

exceptions :: [Interrupt]
exceptions = map f
    [ (-15, "Reset", "Reset [fixed]")
    , (-14, "NMI", "Non maskable interrupt [fixed]")
    , (-13, "HardFault", "All class of fault [fixed]")
    , (-12, "MemManage", "Memory management [settable]")
    , (-11, "BusFault", "Pre-fetch fault, memory access fault [settable]")
    , (-10, "UsageFault", "Undefined instruction or illegal state [settable]")
    , (-5, "SVCall", "System service call via SWI instruction [settable]")
    , (-4, "Debug", "Monitor Debug Monitor [settable]")
    , (-2, "PendSV", "Pendable request for system service [settable]")
    , (-1, "SysTick", "System tick timer [settable]")
    ]
    where f (value, name', description) =
            let name = toUpper name'
             in Interrupt{..}

reserve :: [Either Void Register] -> [Either Reserve Register]
reserve = concat . snd . mapAccumL pad 0 . nubOrdOn addr . sortOn addr
    where pad i (Right r@Register{..})
              | n > 0 = (j, [ Left p, Right r ])
              | n < 0 = error $ "collission at " <> show r
              -- | n < 0 = (j, [ Left $ Reserve "collission" i 0, Right r ])
              | otherwise = (j, [ Right r ])
              where n = addressOffset - i
                    j = i + n + size `div` 8
                    p = Reserve "res" i $ n * 8
          pad _ _ = error "impossible!"
          addr (Right Register{..}) = addressOffset
          addr _ = error "impossible!"

