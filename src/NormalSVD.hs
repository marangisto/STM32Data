{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module NormalSVD (normalSVD, normalize) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.List.Extra (groupSort)
import Data.Hashable
import Numeric (showHex)
import ParseSVD

type Text = T.Text

newtype SVDName = SVDName { unSVDName :: Text } deriving (Eq, Ord, Show)
newtype PeripheralName = PeripheralName { unPeripheralName :: Text } deriving (Eq, Ord, Show)

data Normal = Normal
    { name      :: PeripheralName
    , digest    :: Int
    , address   :: [(Int, [SVDName])]
    }

instance Show Normal where
    show Normal{..} = T.unpack $ T.concat $
        [ unPeripheralName name
        , "["
        , hex $ abs digest
        , "]:"
        , T.intercalate "; " $ map f address
        ]
        where f (a, xs) = hex a <> "=" <> T.intercalate "," (map unSVDName xs)

normalSVD :: [SVD] -> [Normal]
normalSVD xs =
    [ let address = groupSort ys in Normal{..}
    | ((name, digest), ys) <- groupSort $ concatMap plist xs
    ]

plist :: SVD -> [((PeripheralName, Int), (Int, SVDName))]
plist SVD{..} = let svdName = SVDName name in
    [ ((PeripheralName name, hash p), (baseAddress, svdName))
    | p@Peripheral{..} <- peripherals
    ]

normalize :: [SVD] -> IO [Peripheral]
normalize xs = return [ qualify s p | (_, (s, _, p):_) <- normalize' xs ]

qualify :: SVDName -> Peripheral -> Peripheral
qualify s Peripheral{..} =
    let name = unSVDName s <> "_" <> name
     in Peripheral{..}

normalize' :: [SVD] -> [(Int, [(SVDName, PeripheralName, Peripheral)])]
normalize' = groupSort . concatMap f
    where f SVD{..} = let svdName = SVDName name in
            [ (hash p, (svdName, PeripheralName name, p))
            | p@Peripheral{..} <- peripherals
            , Nothing <- [ derivedFrom ]
            ]

pmap :: [SVD] -> Map.Map (SVDName, PeripheralName) Peripheral
pmap = Map.fromList . concatMap f
    where f SVD{..} = let svdName = SVDName name in
            [ ((svdName, PeripheralName name), p)
            | p@Peripheral{..} <- peripherals
            , Nothing <- [ derivedFrom ]
            ]

instance Hashable Peripheral where
    hashWithSalt h Peripheral{..} = hashWithSalt h
--        ( name
--        , sortOn value interrupts
        ( sortOn addressOffset registers
        , derivedFrom
        )

instance Hashable Interrupt where
    hashWithSalt h Interrupt{..} = hashWithSalt h
        ( name
        , value
        )

instance Hashable Register where
    hashWithSalt h Register{..} = hashWithSalt h
        ( name
        , addressOffset
        , resetValue
        , sortOn bitOffset fields
        )

instance Hashable Field where
    hashWithSalt h Field{..} = hashWithSalt h
        ( name
        , bitOffset
        , bitWidth
        )

hex :: Int -> Text
hex x = T.pack $ "0x" ++ showHex x ""

