{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields, TupleSections #-}
module PrettyVector (prettyVector, prettyInterrupt) where

import qualified Data.Text as T
import Data.List.Extra (sortOn, nubOn)
import FrontEnd.ParseSVD
import Utils

prettyVector :: [Interrupt] -> [Text]
prettyVector xs = concat
    [ [ "" ]
    , map weakDecl (tail ys)
    , [ "" ]
    , decl : stack : map ("    , "<>) vs
    , [ "    };" ]
    ]
    where ys = exceptions ++ nubOn value (sortOn value xs)
          vs = map (vectorDecl w) (padInterrupts (-16) ys)
          w = maximum $ 25 : [ 4 + T.length name | Interrupt{..} <- xs ]
          decl = "void (*vectors[])(void) __attribute__ ((section(\".vectors\"))) ="
          stack = mconcat
              [ "    { "
              , "(void(*)(void)) &__estack"
              , T.replicate (w - 5) " "
              , " // -16: Initial stack pointer"
              ]

padInterrupts :: Int -> [Interrupt] -> [Maybe Interrupt]
padInterrupts _ [] = []
padInterrupts i (x:xs)
    = let j = value x
       in replicate (j - i - 1) Nothing ++ [ Just x ] ++ padInterrupts j xs

weakDecl :: Interrupt -> Text
weakDecl Interrupt{..} = mconcat
    [ "template<> void handler<interrupt::"
    , T.toUpper name
    , ">()"
    , " __attribute__ ((weak, alias(\"_Z17__default_handlerv\")));"
    ]

vectorDecl :: Int -> Maybe Interrupt -> Text
vectorDecl _ Nothing = "0x0"
vectorDecl w (Just Interrupt{..}) = mconcat
    [ "handler<interrupt::"
    , T.toUpper name
    , ">"
    , T.replicate (w - T.length name) " "
    , " // "
    , T.pack $ show value
    , ": "
    , cleanWords description
    ]

-- FIXME: these depend on which ARM core we have!
exceptions :: [Interrupt]
exceptions = map (\(value, name, description) -> Interrupt{..})
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

prettyInterrupt :: [Interrupt] -> [Text]
prettyInterrupt xs =
    [ ""
    , "struct interrupt"
    , "{"
    ] ++
    map (T.stripEnd . ("    "<>)) (prettyInterruptBody xs) ++
    [ "};"
    , ""
    ]

prettyInterruptBody :: [Interrupt] -> [Text]
prettyInterruptBody xs =
    [ "static inline void enable() { __asm volatile (\"cpsie i\"); }"
    , "static inline void disable() { __asm volatile (\"cpsid i\"); }"
    , ""
    , "enum interrupt_t"
    ] ++
    zipWith f [(0::Int)..] (exceptions ++ nubOn value (sortOn value xs)) ++
    [ "};"
    , ""
    , "template<interrupt_t INTERRUPT>"
    , "static bool get()"
    , "{"
    , "    return helper<nvic_t, INTERRUPT>::get();"
    , "}"
    , ""
    , "template<interrupt_t INTERRUPT>"
    , "static void set()"
    , "{"
    , "    helper<nvic_t, INTERRUPT>::set();"
    , "}"
    , ""
    , "template<interrupt_t INTERRUPT>"
    , "static void clear()"
    , "{"
    , "    helper<nvic_t, INTERRUPT>::clear();"
    , "}"
    , ""
    , "template<interrupt_t INTERRUPT>"
    , "static bool get_pending()"
    , "{"
    , "    return helper<nvic_t, INTERRUPT>::get_pending();"
    , "}"
    , ""
    , "template<interrupt_t INTERRUPT>"
    , "static void set_pending()"
    , "{"
    , "    helper<nvic_t, INTERRUPT>::set_pending();"
    , "}"
    , ""
    , "template<interrupt_t INTERRUPT>"
    , "static void clear_pending()"
    , "{"
    , "    helper<nvic_t, INTERRUPT>::clear_pending();"
    , "}"
    , ""
    , "template<typename NVIC, interrupt_t I, typename = is_in_range<true>>"
    , "struct helper"
    , "{"
    , "    static_assert(always_false_i<I>::value, \"no such interrupt\");"
    , "};"
    ] ++
    concatMap helperInRange [0..maximum (map value xs) `div` 32]
    where f i Interrupt{..} = mconcat
              [ if i == 0 then "{ " else ", "
              , T.toUpper name
              , " = "
              , T.pack $ show value
              ]

helperInRange :: Int -> [Text]
helperInRange i =
    [ ""
    , "template<typename NVIC, interrupt_t I>"
    , "struct helper<NVIC, I, is_in_range<(" <> rng <> ")>>"
    , "{"
    , "    static bool get() { return " <> reg "ISER" <> " & " <> bit <> "; }"
    , "    static void set() { " <> reg "ISER" <> " = " <> bit <> "; }"
    , "    static void clear() { " <> reg "ICER" <> " = " <> bit <> "; }"
    , "    static bool get_pending() { return " <> reg "ISPR" <> " & " <> bit <> "; }"
    , "    static void set_pending() { " <> reg "ISPR" <> " = " <> bit <> "; }"
    , "    static void clear_pending() { " <> reg "ICPR" <> " = " <> bit <> "; }"
    , "};"
    ]
    where rng = T.pack $ show (i*32) <> " <= I && I < " <> show ((i+1)*32)
          bit = T.pack $ "1 << (I - " <> show (i*32) <> ")"
          reg s = T.pack $ "NVIC::V." <> s <> show i

