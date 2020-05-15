{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Peripheral (peripheralTraitsDecl) where

import qualified Data.Text as T
import Data.List (find, isPrefixOf)
import Data.List.Extra (firstJust)
import Data.Maybe
import Control.Monad
import ParseSVD
import Utils

peripheralTraitsDecl :: [Peripheral] -> [Text]
peripheralTraitsDecl ps 
    | Just rcc <- find (\Peripheral{..} -> name == "RCC") ps
    = let regs = [ r | r@Register{..} <- registers rcc, pred name ]
       in [ ""
          , "template<typename PERIPHERAL> struct peripheral_traits {};"
          , ""
          ] ++ concat (mapMaybe (fmap prettyPeripheralTraits . peripheralMethods regs) ps)
    | otherwise = []
    where pred s = any (`T.isPrefixOf` s) [ "AHB", "APB", "IOP" ]

prettyPeripheralTraits :: (Text, [Text]) -> [Text]
prettyPeripheralTraits (pname, traits) =
    [ "template<> struct peripheral_traits<" <> T.toLower pname <> "_t>"
    , "{"
    ] ++
    map ("    " <>) traits ++
    [ "};"
    , ""
    ]

peripheralMethods :: [Register] -> Peripheral -> Maybe (Text, [Text])
peripheralMethods regs Peripheral{..}
    = (\xs -> if null xs then Nothing else Just (name, xs))
    $ maybe [] (uncurry f) (firstJust (findRegisterFields [ "EN", "1EN", "" ] $ h name) regs)
   ++ maybe [] (uncurry g) (firstJust (findRegisterFields [ "RST", "1RST" ] $ h name) regs)
    where g registerName fieldName =
            [ prettyMethod "reset" True registerName fieldName
            ]
          f registerName fieldName =
            [ prettyMethod "enable" True registerName fieldName
            , prettyMethod "disable" False registerName fieldName
            ]
          h "ADC12_COMMON" = "ADC12"
          h "SYSCFG_COMP" = "SYSCFG"
          h x = x

findRegisterFields :: [Text] -> Text -> Register -> Maybe (Text, Text)
findRegisterFields ss p r
    = foldl1 mplus $ map (\s -> findRegisterField s p r) ss

findRegisterField :: Text -> Text -> Register -> Maybe (Text, Text)
findRegisterField suffix pname Register{..}
    = (name,) <$> findFieldName suffix name fields

findFieldName :: Text -> Text -> [Field] -> Maybe Text
findFieldName suffix pname xs
    = (\Field{..} -> name) <$> find p xs
    where p Field{..} 
              | "GPIO" `T.isPrefixOf` pname
              , "IOP" `T.isPrefixOf` name
              = "IOP" <> T.pack [ T.last pname ] <> suffix == name
              | otherwise = pname <> suffix == name

prettyMethod :: Text -> Bool -> Text -> Text -> Text
prettyMethod methodName setbit registerName fieldName = mconcat
    [ "static void "
    , methodName
    , "() { RCC."
    , registerName
    , if setbit then " |= " else " &= ~"
    , "rcc_t::"
    , registerName
    , "_" 
    , fieldName
    , "; }"
    ]

