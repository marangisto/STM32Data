{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module NormalSVD (normalSVD) where

import qualified Data.Text as T
import Data.List.Extra (groupSort)
import Data.Hashable
import ParseSVD

type Text = T.Text

newtype SVDName = SVDName { unSVDName :: Text } deriving (Eq, Ord, Show)
newtype PeripheralName = PeripheralName { unPeripheralName :: Text } deriving (Eq, Ord, Show)

data Normal = Normal
    { name      :: PeripheralName
    , address   :: [(Int, [SVDName])]
    } deriving (Show)

normalSVD :: [SVD] -> [Normal]
normalSVD xs =
    [ let address = groupSort ys in Normal{..}
    | (name, ys) <- groupSort $ concatMap plist xs
    ]

plist :: SVD -> [(PeripheralName, (Int, SVDName))]
plist SVD{..} = let svdName = SVDName name in
    [ (PeripheralName name, (baseAddress, svdName))
    | Peripheral{..} <- peripherals
    ]

