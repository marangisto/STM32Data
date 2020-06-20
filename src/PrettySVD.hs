{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module PrettySVD (prettyPeripheral, peripheralMap) where

import qualified Data.Text as T
import Data.List (sortOn)
import Data.List.Extra (nubOn, groupSortOn)
import Data.Bits (shift)
import Data.Maybe (isJust)
import FrontEnd.ParseSVD
import Utils

prettyPeripheral :: Text -> Peripheral -> [Text]
prettyPeripheral svd Peripheral{derivedFrom=Just from,..} =
    [ ""
    , "typedef " <> tname (Just svd) from
    <> " " <> tname Nothing name <> ";"
    ]
prettyPeripheral svd Peripheral{..} =
    banner [ cleanWords description ] ++
    [ ""
    , "struct " <> tname (Just svd) name
    , "{"
    ] ++
    concatMap (register w) (reserve $ fixupRegisters registers) ++
    concatMap registerFields registers ++
    [ "};"
    ]
    where w = maximum [ T.length name | Register{..} <- registers ]

tname :: Maybe Text -> Text -> Text
tname qual name = T.toLower $ maybe "" (<>"_") qual <> name <> "_t"

fixupRegisters :: [Register] -> [Register]
fixupRegisters = map f . groupSortOn addressOffset
    where f [x] = x
          f xs@(x:_) = g x
              { fields = nubOn (\Field{..} -> name)
                       $ sortOn (\Field{..} -> name)
                       $ concatMap fields xs
              }
          g r@Register{..}
              | Just s <- T.stripSuffix "_Output" name
              = Register{name = s,..}
              | Just s <- T.stripSuffix "_Input" name
              = Register{name = s,..}
              | otherwise = r
 
reserve :: [Register] -> [(Register, Maybe Int)]
reserve xs = zip xs $ ys ++ [ Nothing ]
    where ys = map (Just . addressOffset) $ tail xs

register :: Int -> (Register, Maybe Int) -> [Text]
register w (Register{..}, nextOffset) = T.concat
    [ "    "
    , "volatile uint32_t"
    , " "
    , name
    , ";"
    , T.replicate (w - T.length name) " "
    , " // " <> maybe "" (\t -> "[" <> t <> "] ") access
    , cleanWords description
    ] : (maybe [] (:[]) $ pad name addressOffset nextOffset)

pad :: Text -> Int -> Maybe Int -> Maybe Text
pad name addr next
    | Just n <- next, n < addr + 4
    = error $ "collission at register " <> T.unpack name
    -- = Just $ "collission at register " <> name
    | Just n <- next, n > addr + 4 = Just $ T.concat
        [ "    "
        , "reserved_t<"
        , hex $ (n - (addr + 4)) `div` 4
        , "> _"
        , hex n
        , ";"
        ]
    | otherwise = Nothing

registerFields :: Register -> [Text]
registerFields Register{..} = "" : rv : xs
    where xs = concatMap (field w name) fields
          w = maximum $ map fieldWidth fields
          rv = T.concat
            [ "    "
            , "static constexpr uint32_t"
            , " "
            , name
            , "_RESET_VALUE"
            , " = "
            , hex resetValue
            , ";"
            ]

field :: Int -> Text -> Field -> [Text]
field w regName f@Field{..}
    | bitWidth == 32 = []                   -- trivial field
    | bitWidth == 1 = (:[]) $ T.concat      -- single-bit
        [ decl
        , " = "
        , bitConstant bitOffset
        , ";"
        , doc
        ]
    | otherwise =                           -- multi-bit
        [ "    " <> "template<uint32_t X>"
        , decl <> " =  " <> doc
        , "        " <> bits <> ";"
        ]
    where decl = T.concat
              [ "    "
              , "static constexpr uint32_t"
              , " "
              , regName
              , "_"
              , name
              ]
          doc = T.concat
              [ T.replicate (w - fieldWidth f) " "
              , " // "
              , cleanWords description
              ]
          bits = T.concat
              [ "bit_field_t<"
              , T.pack $ show bitOffset
              , ", "
              , hex $ shift 0xffffffff (bitWidth - 32)
              , ">::value<X>()"
              ]

fieldWidth :: Field -> Int
fieldWidth Field{..}
    | bitWidth == 32 = 0
    | bitWidth == 1 = T.length $ name <> bitConstant bitOffset
    | otherwise = T.length $ name

bitConstant :: Int -> Text
bitConstant = hex . shift 1

peripheralMap :: SVD -> [Text]
peripheralMap SVD{..} =
    [ name <> "," <> T.pack (show baseAddress)
    | Peripheral{..} <- sortOn baseAddress peripherals
    ]

