{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module FrontEnd.Fixup (Reserve(..), fixup, fixupInstanceName) where

import FrontEnd.Normalize as NormalSVD (NormalSVD(..))
import FrontEnd.Normalize as PeriphType (PeriphType(..))
import FrontEnd.Normalize as PeriphRef (PeriphRef(..))
import FrontEnd.Normalize as PeriphInst (PeriphInst(..))
import FrontEnd.Normalize as Register (Register(..))
import FrontEnd.Normalize as Field (Field(..))
import FrontEnd.Normalize as Interrupt (Interrupt(..))
import Utils
import Data.Text
    ( isPrefixOf, isSuffixOf, toUpper
    , stripPrefix, stripSuffix, dropWhileEnd
    )
import qualified Data.Text as T
import Data.List.Extra (nubOrdOn, groupSortOn)
import Data.List (sortOn, mapAccumL)
import Data.Char (isDigit)
import Data.Void

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
runPeriphTypeEdits xs f = foldl (.) id (map ($ f) xs)

runPeriphInstEdits :: [PeriphInstEdit] -> PeriphInstEdit
runPeriphInstEdits xs f p = foldl (.) id (map (($ p) . ($ f)) xs)

runRegisterEdits :: [RegisterEdit] -> RegisterEdit
runRegisterEdits xs f p = foldl (.) id (map (($ p) . ($ f)) xs)

runFieldEdits :: [FieldEdit] -> FieldEdit
runFieldEdits xs f p r = foldl (.) id (map (($ r) . ($ p) . ($ f)) xs)

periphTypeEdits :: [PeriphTypeEdit]
periphTypeEdits =
    [ usbRegs
    , fixGroupName
    , sysTick
    , missingInst
    ]

periphInstEdits :: [PeriphInstEdit]
periphInstEdits =
    [ instName
    , baseAddr
    ]

registerEdits :: [RegisterEdit]
registerEdits =
    [ regName
    , gpioFields
    , timCcmr
    , adcHwcfgr6
    , nvicIserx
    , syscfgPrefix
    , rccPllcfgr
    , gpioReg
    , usartCr1
    , rccApb
    ]

fieldEdits :: [FieldEdit]
fieldEdits =
    [ compxCsr
    , rccFields
    , adcFields
    , syscfgFields
    , timFields
    ]

usbRegs :: PeriphTypeEdit
usbRegs _ x@PeriphType{typeRef=PeriphRef{..},..}
    | name == "USB" = x { registers = filter (not . usbBuffer) registers }
    | otherwise = x

fixGroupName :: PeriphTypeEdit
fixGroupName fam x@PeriphType{typeRef=PeriphRef{..},..}
    | groupName == "USART", "LPUART" `isPrefixOf` name
    = x { groupName = "LPUART" }
    | groupName == "USART", "LPUSART" `isPrefixOf` name, fam == "STM32L0"
    = x { groupName = "LPUSART" }
    | groupName == "USART", "UART" `isPrefixOf` name, fam `notElem` [ "STM32H7", "STM32L4" ]
    = x { groupName = "UART" }
    | groupName == "FSMC", name == "FMC"
    = x { groupName = "FMC" }
    | otherwise = x

sysTick :: PeriphTypeEdit
sysTick _ x@PeriphType{typeRef=PeriphRef{..},..}
    | name == "STK" = x { registers = map (fmap f) registers }
    | otherwise = x
    where f :: Register -> Register
          f r@Register{..}
            | name == "CTRL" = r { Register.name = "CSR" }
            | name == "LOAD" = r { Register.name = "RVR" }
            | name == "LOAD_" = r { Register.name = "RVR" }
            | name == "VAL" = r { Register.name = "CVR" }
            | otherwise = r

missingInst :: PeriphTypeEdit
missingInst fam x@PeriphType{typeRef=PeriphRef{..},..}
    | fam == "STM32H7", name == "DAC"
    = x -- FIXME: try to insert missing second instance here!
    | fam == "STM32G0", name `elem` [ "STK", "NVIC" ]
    = let inst = head $ periphInsts
          ref = instRef inst
          miss = [ inst { instRef = ref { svd = "STM32G0" <> s } } | s <- [ "B0", "B1", "C1" ] ]
       in x { periphInsts = periphInsts ++ miss }
    | otherwise = x

instName :: PeriphInstEdit
instName fam _ x@PeriphInst{instRef=r@PeriphRef{..}}
    = x { instRef = r { PeriphRef.name = fixupInstanceName fam name } }

fixupInstanceName :: Text -> Text -> Text
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

baseAddr :: PeriphInstEdit
baseAddr "STM32G4" "ADC12_COMMON" x@PeriphInst{..}
    | baseAddress == 0x50000200 = x { baseAddress = 0x50000300 }
    | otherwise = x
baseAddr _ _ x = x

regName :: RegisterEdit
regName "STM32F7" "RCC" x@Register{name="DKCFGR1"}
    = x { Register.name = "DCKCFGR1" }
regName "STM32L1" p x@Register{name="OSPEEDER"}
    | "GPIO" `isPrefixOf` p = x { Register.name = "OSPEEDR" }
    | otherwise = x
regName "STM32G0" "RCC" x@Register{name="PLLCFGR"}
    = x { Register.name = "PLLSYSCFGR" }
regName "STM32G0" p x@Register{..}
    | Just rest <- stripPrefix (p <> "_") name = x { Register.name = rest }
    | Just rest <- stripSuffix "_FIFO_ENABLED" name = x { Register.name = rest }
    | otherwise = x
regName "STM32G4" p x@Register{..}
    | Just rest <- stripPrefix (p <> "_") name = x { Register.name = rest }
    | Just rest <- stripPrefix "DAC_" name = x { Register.name = rest }
    | otherwise = x
regName "STM32H7" p x@Register{..}
    | Just rest <- stripPrefix (p <> "_") name
    , p == "PWR"
    = x { Register.name = rest }
regName _ _ x = x

gpioFields :: RegisterEdit
gpioFields "STM32F0" "RCC" x@Register{fields=fs,..}
    | name == "AHBENR" = x { fields = sortOn bitOffset $ en : fs }
    | name == "AHBRSTR" = x { fields = sortOn bitOffset $ rst : fs }
    | otherwise = x
    where en = Field "IOPEEN" "I/O port E clock enable" 21 1
          rst = Field "IOPERST" "I/O port E reset" 21 1
gpioFields _ _ x = x

timCcmr :: RegisterEdit
timCcmr _ p x@Register{..}
    | tim, Just s <- stripSuffix "_INPUT" name = x { Register.name = s }
    | tim, Just s <- stripSuffix "_OUTPUT" name = x { Register.name = s }
    | otherwise = x
    where tim = "TIM" `isPrefixOf` p

adcHwcfgr6 :: RegisterEdit
adcHwcfgr6 _ p x@Register{..}
    | p == "ADC", name == "HWCFGR6"
    , resetValue == fromHex "0x1f1f1f11f"
    = x { resetValue = fromHex "0x1f1f1f1f" }
    | otherwise = x

nvicIserx :: RegisterEdit
nvicIserx _ p x@Register{..}
    | p == "NVIC", name `elem` [ "ISER", "ICER", "ISPR", "ICPR" ]
    = x { Register.name = name <> "0" }
    | otherwise = x

syscfgPrefix :: RegisterEdit
syscfgPrefix f "SYSCFG" x@Register{..}
    | f `elem` [ "STM32F0", "STM32F3" ]
    , Just rest <- stripPrefix "SYSCFG_" name
    = x { Register.name = rest }
    | otherwise = x
syscfgPrefix _ _ x = x

rccPllcfgr :: RegisterEdit
rccPllcfgr family "RCC" x@Register{..}
    | family `elem` [ "STM32F4", "STM32F7", "STM32F2" ]
    , name `elem` [ "PLLCFGR", "CFGR" ] = x { fields = f fields }
    | otherwise = x
    where f :: [Field] -> [Field]
          f = sortOn bitOffset . map h
            . groupSortOn (\Field{..} -> name) . map g
          g :: Field -> Field
          g x@Field{..}
              | any (`isPrefixOf` name) [ "PLL", "SW" ]
              = x { Field.name = dropWhileEnd isDigit name }
              | name `elem` [ "MCO1EN", "MCO2EN" ] = x
              | "MCO1PRE" `isPrefixOf` name = x { Field.name = "MCO1PRE" }
              | "MCO2PRE" `isPrefixOf` name = x { Field.name = "MCO2PRE" }
              | "MCO1" `isPrefixOf` name = x { Field.name = "MCO1SEL" }
              | "MCO2" `isPrefixOf` name = x { Field.name = "MCO2SEL" }
              | otherwise = x
          h :: [Field] -> Field
          h [] = error "impossible!"
          h (x:[]) = x
          h xs@(x:_) = x
              { bitOffset = minimum $ map (\Field{..} -> bitOffset) xs
              , bitWidth = length xs
              }
rccPllcfgr _ _ x = x

gpioReg :: RegisterEdit
gpioReg "STM32F7" p x@Register{..}
    | "GPIO" `isPrefixOf` p, name == "GPIOB_OSPEEDR"
    = x { Register.name = "OSPEEDR" }
    | otherwise = x
gpioReg _ _ x = x

usartCr1 :: RegisterEdit
usartCr1 "STM32L4" p x@Register{..}
    | "USART" `isPrefixOf` p, name == "0X00000000"
    = x { Register.name = "CR1", displayName = "CR1" }
    | otherwise = x
usartCr1 _ _ x = x

rccApb :: RegisterEdit
rccApb "STM32G0" "RCC" x@Register{..} 
    | Just rest <- stripPrefix "APB" name
    , isDigit (T.last rest)
    = x { Register.name = T.snoc "APB" (T.last rest) <> T.init rest }
    | otherwise = x
rccApb _ _ x = x

compxCsr :: FieldEdit
compxCsr f p r x@Field{..}
    | p == "SYSCFG_COMP_OPAMP", "COMP" `isPrefixOf` r
    , "_CSR" `isSuffixOf` r, "INMSEL" `isSuffixOf` name
    , bitOffset == 22 = x { Field.name = name <> "3" }
    | f == "STM32F3", p == "SYSCFG", "COMP" `isPrefixOf` r
    , "_CSR" `isSuffixOf` r, "INMSEL" `isSuffixOf` name
    , bitOffset == 22 = x { Field.name = name <> "3" }
    | otherwise = x

rccFields :: FieldEdit
rccFields fam "RCC" _ x@Field{..}
    | name == "AD12CSMEN" = x { Field.name = "ADC12_COMMONSMEN" }
    | name == "ADC12EN" = x { Field.name = "ADC12_COMMONEN" }
    | name == "ADC12SMEN" = x { Field.name = "ADC12_COMMONSMEN" }
    | name == "ADC12RST" = x { Field.name = "ADC12_COMMONRST" }
    | name == "ADC345EN" = x { Field.name = "ADC345_COMMONEN" }
    | name == "ADC345SMEN" = x { Field.name = "ADC345_COMMONSMEN" }
    | name == "ADC345RST" = x { Field.name = "ADC345_COMMONRST" }
    | name == "DMAMUX1EN" = x { Field.name = "DMAMUXEN" }
    | name == "DMAMUX1SMEN" = x { Field.name = "DMAMUXSMEN" }
    | name == "DMAMUX1RST" = x { Field.name = "DMAMUXRST" }
    | name == "CRYPTEN" = x { Field.name = "AESEN" }
    | name == "CRYPTSMEN" = x { Field.name = "AESSMEN" }
    | name == "CRYPTRST" = x { Field.name = "AESRST" }
    | name == "HRTIMEREN" = x { Field.name = "HRTIM_MASTEREN" }
    | name == "HRTIMERSMEN" = x { Field.name = "HRTIM_MASTERSMEN" }
    | name == "HRTIM1RST" = x { Field.name = "HRTIM_MASTERRST" }
    | name == "SP3EN" = x { Field.name = "SPI3EN" }
    | name == "SP3SMEN" = x { Field.name = "SPI3SMEN" }
    | name == "SP3RST" = x { Field.name = "SPI3RST" }
    | name == "USBDEN" = x { Field.name = "USBEN" }
    | name == "USBDSMEN" = x { Field.name = "USBSMEN" }
    | name == "USBDRST" = x { Field.name = "USBRST" }
    | name == "USBPDEN" = x { Field.name = "UCPD1EN" }
    | name == "USBPDSMEN" = x { Field.name = "UCPD1SMEN" }
    | name == "USBPDRST" = x { Field.name = "UCPD1RST" }
    | name == "FLITFRST" = x { Field.name = "FLASHRST" }
    | name == "FLITFEN" = x { Field.name = "FLASHEN" }
    | name == "FLITFSMEN" = x { Field.name = "FLASHSMEN" }
    | name == "ADC345RST_" = x { Field.name = "ADC345_COMMONRST" }
    | name == "DAC1RST_" = x { Field.name = "DAC1RST" }
    | name == "FLITFRST_" = x { Field.name = "FLASHRST" }
    | name == "ADCEN" = x { Field.name = "ADC1EN" }
    | name == "ADCRST" = x { Field.name = "ADC1RST" }
    | name == "ADCSMEN" = x { Field.name = "ADC1SMEN" }
    | name == "DMAEN" = x { Field.name = "DMA1EN" }
    | name == "DMARST" = x { Field.name = "DMA1RST" }
    | name == "DMASMEN" = x { Field.name = "DMA1SMEN" }
    | name == "DAC1" = x { Field.name = "DAC1EN" }
    | name == "DAC2" = x { Field.name = "DAC2EN" }
    | name == "DAC3" = x { Field.name = "DAC3EN" }
    | name == "DAC4" = x { Field.name = "DAC4EN" }
    | fam == "STM32L1"
    , Just s <- stripPrefix "GPIOP" name
        = x { Field.name = "GPIO" <> s }
    | fam == "STM32G0"
    , Just s <- stripPrefix "GPIO" name
        = x { Field.name = "IOP" <> s }
    | otherwise = x
rccFields _ _ _ x = x

adcFields :: FieldEdit
adcFields f p r x@Field{..}
    | f == "STM32G4", "ADC" `isPrefixOf` p, r == "CFGR"
    , name == "EXTSEL", bitOffset == 6, bitWidth == 4
    = x { bitOffset = 5, bitWidth = 5 }
    | otherwise = x

usbBuffer :: Either Void Register -> Bool
usbBuffer (Right Register{..})
    | "ADDR" `isPrefixOf` name = True
    | "COUNT" `isPrefixOf` name = True
    | otherwise = False
usbBuffer _ = False

syscfgFields :: FieldEdit
syscfgFields "STM32H7" "SYSCFG" "PWRCR" x@Field{..}
    | name == "ODEN", bitWidth /= 1 = x { bitWidth = 1 }
    | otherwise = x
syscfgFields _ _ _ x = x

timFields :: FieldEdit
timFields _ _ "CCMR1_INPUT" x@Field{..}
    | name `elem` [ "ICPCS", "IC1PCS" ] = x { Field.name = "IC1PSC" }
    | name == "IC2PCS" = x { Field.name = "IC2PSC" }
    | otherwise = x
timFields _ _ "CCMR1_OUTPUT" x@Field{..}
    | name == "CC2S", bitWidth == 1 = x { bitWidth = 2 }
    | otherwise = x
timFields _ _ "CCMR2_OUTPUT" x@Field{..}
    | name == "CC4S", bitWidth == 1 = x { bitWidth = 2 }
    | otherwise = x
timFields _ _ _ x = x

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
            | Just i <- stripPrefix "TIM2_" name = x { Interrupt.name = "TIM" <> i }
            | otherwise = x
          missing = map (\(name, value, description) -> Interrupt{..})
            [ ("TIM6", 49, "TIM6 global interrupt")
            , ("TIM7", 50, "TIM7 global interrupt")
            ]
fixupInterrupts _ = map f
    where f :: Interrupt -> Interrupt
          f x@Interrupt{..}
            | Just rest <- stripSuffix "_IRQ" name = x { Interrupt.name = rest }
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

