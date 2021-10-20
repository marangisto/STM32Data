{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module FrontEnd.Fixup (Reserve(..), fixup, fixupInstanceName) where

import FrontEnd.Normalize
import Utils
import Data.Text (isPrefixOf, isSuffixOf, toUpper)
import Data.Text (stripPrefix, stripSuffix, dropWhileEnd)
import qualified Data.Text as T
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
    , interrupts = exceptions ++ fixupInterrupts family interrupts
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
    , missing_inst
    ]

periphInstEdits :: [PeriphInstEdit]
periphInstEdits =
    [ inst_name
    , base_addr
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
    , rcc_apb
    ]

fieldEdits :: [FieldEdit]
fieldEdits =
    [ compx_csr
    , rcc_fields
    , adc_fields
    , syscfg_fields
    , tim_fields
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

missing_inst :: PeriphTypeEdit
missing_inst fam x@PeriphType{typeRef=PeriphRef{..},..}
    | fam == "STM32H7", name == "DAC"
    = x -- FIXME: try to insert missing second instance here!
    | fam == "STM32G0", name `elem` [ "STK", "NVIC" ]
    = let inst = head $ periphInsts
          ref = instRef inst
          miss = [ inst { instRef = ref { svd = "STM32G0" <> s } } | s <- [ "B0", "B1", "C1" ] ]
       in x { periphInsts = periphInsts ++ miss }
    | otherwise = x

inst_name :: PeriphInstEdit
inst_name fam _ x@PeriphInst{instRef=r@PeriphRef{..}}
    = x { instRef = r { name = fixupInstanceName fam name } }

fixupInstanceName fam name
    | name == "PF" = "PF_"
    | name == "LPTIMER1" = "LPTIM1"
    | name == "CEC" = "HDMI_CEC"
    | name == "USB_FS_DEVICE" = "USB"
    | name `elem` [ "DAC", "LPUART", "SAI", "QUADSPI", "ADC", "DMA" ]
        = name <> "1"
    | fam == "STM32L1", Just s <- stripPrefix "UART" name
        = "USART" <> s
    | otherwise = name

base_addr :: PeriphInstEdit
base_addr "STM32G4" "ADC12_COMMON" x@PeriphInst{..}
    | baseAddress == 0x50000200 = x { baseAddress = 0x50000300 }
    | otherwise = x
base_addr _ _ x = x

reg_name :: RegisterEdit
reg_name "STM32F7" "RCC" x@Register{name="DKCFGR1"}
    = x { name = "DCKCFGR1" }
reg_name "STM32L1" p x@Register{name="OSPEEDER"}
    | "GPIO" `isPrefixOf` p = x { name = "OSPEEDR" }
    | otherwise = x
reg_name "STM32G0" "RCC" x@Register{name="PLLCFGR"}
    = x { name = "PLLSYSCFGR" }
reg_name "STM32G0" p x@Register{..}
    | Just rest <- stripPrefix (p <> "_") name = x { name = rest }
    | otherwise = x
reg_name "STM32G4" p x@Register{..}
    | Just rest <- stripPrefix (p <> "_") name = x { name = rest }
    | Just rest <- stripPrefix ("DAC_") name = x { name = rest }
    | otherwise = x
reg_name "STM32H7" p x@Register{..}
    | Just rest <- stripPrefix (p <> "_") name
    , p `elem` [ "PWR" ]
    = x { name = rest }
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
syscfg_prefix f "SYSCFG" x@Register{..}
    | f `elem` [ "STM32F0", "STM32F3" ]
    , Just rest <- stripPrefix "SYSCFG_" name
    = x { name = rest }
    | otherwise = x
syscfg_prefix _ _ x = x

rcc_pllcfgr :: RegisterEdit
rcc_pllcfgr family "RCC" x@Register{..}
    | family `elem` [ "STM32F4", "STM32F7", "STM32F2" ]
    , name `elem` [ "PLLCFGR", "CFGR" ] = x { fields = f fields }
    | otherwise = x
    where f :: [Field] -> [Field]
          f = sortOn bitOffset . map h
            . groupSortOn (\Field{..} -> name) . map g
          g :: Field -> Field
          g x@Field{..}
              | any (`isPrefixOf` name) [ "PLL", "SW" ]
              = x { name = dropWhileEnd isDigit name }
              | name `elem` [ "MCO1EN", "MCO2EN" ] = x
              | "MCO1PRE" `isPrefixOf` name = x { name = "MCO1PRE" }
              | "MCO2PRE" `isPrefixOf` name = x { name = "MCO2PRE" }
              | "MCO1" `isPrefixOf` name = x { name = "MCO1SEL" }
              | "MCO2" `isPrefixOf` name = x { name = "MCO2SEL" }
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

rcc_apb :: RegisterEdit
rcc_apb "STM32G0" "RCC" x@Register{..} 
    | Just rest <- stripPrefix "APB" name
    , isDigit (T.last rest)
    = x { name = T.snoc "APB" (T.last rest) <> T.init rest }
    | otherwise = x
rcc_apb _ _ x = x

compx_csr :: FieldEdit
compx_csr f p r x@Field{..}
    | p == "SYSCFG_COMP_OPAMP", "COMP" `isPrefixOf` r
    , "_CSR" `isSuffixOf` r, "INMSEL" `isSuffixOf` name
    , bitOffset == 22 = x { name = name <> "3" }
    | f == "STM32F3", p == "SYSCFG", "COMP" `isPrefixOf` r
    , "_CSR" `isSuffixOf` r, "INMSEL" `isSuffixOf` name
    , bitOffset == 22 = x { name = name <> "3" }
    | otherwise = x

rcc_fields :: FieldEdit
rcc_fields fam "RCC" _ x@Field{..}
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
    | name == "ADCEN" = x { name = "ADC1EN" }
    | name == "ADCRST" = x { name = "ADC1RST" }
    | name == "ADCSMEN" = x { name = "ADC1SMEN" }
    | name == "DMAEN" = x { name = "DMA1EN" }
    | name == "DMARST" = x { name = "DMA1RST" }
    | name == "DMASMEN" = x { name = "DMA1SMEN" }
    | name == "DAC1" = x { name = "DAC1EN" }
    | name == "DAC2" = x { name = "DAC2EN" }
    | name == "DAC3" = x { name = "DAC3EN" }
    | name == "DAC4" = x { name = "DAC4EN" }
    | fam == "STM32L1"
    , Just s <- stripPrefix "GPIOP" name
        = x { name = "GPIO" <> s }
    | fam == "STM32G0"
    , Just s <- stripPrefix "GPIO" name
        = x { name = "IOP" <> s }
    | otherwise = x
rcc_fields _ _ _ x = x

adc_fields :: FieldEdit
adc_fields f p r x@Field{..}
    | f == "STM32G4", "ADC" `isPrefixOf` p, r == "CFGR"
    , name == "EXTSEL", bitOffset == 6, bitWidth == 4
    = x { bitOffset = 5, bitWidth = 5 }
    | otherwise = x

usb_buffer :: Either Void Register -> Bool
usb_buffer (Right Register{..})
    | "ADDR" `isPrefixOf` name = True
    | "COUNT" `isPrefixOf` name = True
    | otherwise = False
usb_buffer _ = False

syscfg_fields :: FieldEdit
syscfg_fields "STM32H7" "SYSCFG" "PWRCR" x@Field{..}
    | name == "ODEN", bitWidth /= 1 = x { bitWidth = 1 }
    | otherwise = x
syscfg_fields _ _ _ x = x

tim_fields :: FieldEdit
tim_fields _ _ "CCMR1_INPUT" x@Field{..}
    | name `elem` [ "ICPCS", "IC1PCS" ] = x { name = "IC1PSC" }
    | name == "IC2PCS" = x { name = "IC2PSC" }
    | otherwise = x
tim_fields _ _ "CCMR1_OUTPUT" x@Field{..}
    | name == "CC2S", bitWidth == 1 = x { bitWidth = 2 }
    | otherwise = x
tim_fields _ _ "CCMR2_OUTPUT" x@Field{..}
    | name == "CC4S", bitWidth == 1 = x { bitWidth = 2 }
    | otherwise = x
tim_fields _ _ _ x = x

{-
fixupPeriphType "STM32L4" p@PeriphType{..}
    | name == "USART1"
    = p { registers = map usart1_cr1 registers }
fixupPeriphType _ p@PeriphType{name="SEC_DAC",derivedFrom=Just "DAC",..}
    = p { name = "DAC2", derivedFrom = Just "DAC1" }
-}

fixupInterrupts :: Text -> [Interrupt] -> [Interrupt]
fixupInterrupts "STM32L5" = (++missing) . map f
    where f :: Interrupt -> Interrupt
          f x@Interrupt{..}
            | Just i <- stripPrefix "TIM2_" name = x { name = "TIM" <> i }
            | otherwise = x
          missing = map (\(name, value, description) -> Interrupt{..})
            [ ("TIM6", 49, "TIM6 global interrupt")
            , ("TIM7", 50, "TIM7 global interrupt")
            ]
fixupInterrupts _ = map f
    where f :: Interrupt -> Interrupt
          f x@Interrupt{..}
            | Just rest <- stripSuffix "_IRQ" name = x { name = rest }
            | otherwise = x

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
reserve = concat . snd . mapAccumL pad 0 . combineFlags
    where pad i (Right r@Register{..})
              | n > 0 = (j, [ Left p, Right r ])
              | n < 0 = error $ "collission at " <> show r
              -- | n < 0 = (j, [ Left $ Reserve "collission" i 0, Right r ])
              | otherwise = (j, [ Right r ])
              where n = addressOffset - i
                    j = i + n + size `div` 8
                    p = Reserve "res" i $ n * 8
          pad _ _ = error "impossible!"

combineFlags :: [Either Void Register] -> [Either Void Register]
combineFlags = map comb . groupSortOn addr
    where addr (Right Register{..}) = addressOffset
          addr _ = error "impossible!"
          name (Field{..}) = name
          comb :: [Either Void Register] -> Either Void Register
          comb (x:[]) = x
          comb xs@(Right x:_) = Right x
              { fields = nubOrdOn name $ sortOn name
                       $ concatMap (either (const []) fields) xs
              }
          comb _ = error "unexpected lack of registers"

