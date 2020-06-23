{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
module FrontEnd.Normalize
    ( NormalSVD(..)
    , PeriphType(..)
    , PeriphRef(..)
    , PeriphInst(..)
    , Register(..)
    , Field(..)
    , Interrupt(..)
    , normalize
    , peripheralNames
    ) where

import FrontEnd.ParseSVD
import Data.Void
import Data.Hashable
import Data.Ord (Down(..))
import Data.List (nub, sort, sortOn, partition)
import Data.List.Extra (groupSort, groupSortOn)
import Data.Maybe (fromMaybe, isNothing)
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Arrow (second)
import Utils

data NormalSVD cc = NormalSVD
    { family        :: !Text
    , periphTypes   :: ![PeriphType]
    , interrupts    :: ![Interrupt]
    , clockControl  :: !cc
    } deriving (Show)

data PeriphType = PeriphType
    { typeRef       :: !PeriphRef
    , description   :: !Text
    , groupName     :: !Text
    , registers     :: ![Register]
    , periphInsts   :: ![PeriphInst]
    } deriving (Show)

data PeriphRef = PeriphRef
    { svd           :: !Text
    , name          :: !Text
    } deriving (Eq, Ord, Show)

data PeriphInst = PeriphInst
    { instRef       :: !PeriphRef
    , baseAddress   :: !Int
    } deriving (Show)

type Index = Map.Map PeriphRef PeriphRef

normalize :: Text -> [SVD] -> NormalSVD ()
normalize family xs = NormalSVD{..}
    where periphTypes = mergeInstances moreInsts periphTypes'
          interrupts = mergeInterrupts $ concat [ interrupts | SVD{..} <- xs ]
          clockControl = ()
          (outright, derived) = partition (isOutright . snd) allPerips
          isOutright = isNothing . derivedFrom
          allPerips = [ (name, p) | SVD{..} <- xs, p <- peripherals ]
          canon = groupSortOn (hash . snd) outright
          periphTypes' = map periphType canon
          moreInsts = map (fromDerived $ index periphTypes') derived

mergeInstances
    :: [(PeriphRef, PeriphInst)]
    -> [PeriphType]
    -> [PeriphType]
mergeInstances xs = sortOn h . map f
    where f t@PeriphType{..} = t
              { periphInsts = sortOn instRef $ periphInsts ++ g typeRef
              }
          g typeRef = fromMaybe [] $ lookup typeRef $ groupSort xs
          h PeriphType{typeRef=PeriphRef{..}} = (nameNum name, svd)

fromDerived :: Index -> (Text, Peripheral) -> (PeriphRef, PeriphInst)
fromDerived index (svd, Peripheral{..}) = (typeRef, PeriphInst{..})
    where instRef = PeriphRef{..}
          typeRef = case Map.lookup periphRef index of
              Just x -> x
              _ -> error $ "failed to derive " <> show instRef
          periphRef = PeriphRef svd $ fromMaybe (error "!!!") derivedFrom

index :: [PeriphType] -> Index
index = Map.fromList . concatMap f
    where f PeriphType{..} = map (g typeRef) periphInsts
          g typeRef PeriphInst{..} = (instRef, typeRef)

periphType :: [(Text, Peripheral)] -> PeriphType
periphType xs@((svd, Peripheral{..}):_) = PeriphType{..}
    where periphInsts = map (periphInst typeRef) xs
          typeRef = PeriphRef{..}

periphInst :: PeriphRef -> (Text, Peripheral) -> PeriphInst
periphInst typeRef (svd, Peripheral{..}) = PeriphInst{..}
    where instRef = PeriphRef{..}

mergeInterrupts :: [Interrupt] -> [Interrupt]
mergeInterrupts = map f . groupSortOn value
    where f = head . sortOn (Down . T.length . \Interrupt{..} -> name)

peripheralNames :: NormalSVD a -> [Text]
peripheralNames NormalSVD{..} = nub $ sort
    [ name
    | PeriphType{..} <- periphTypes
    , PeriphInst{..} <- periphInsts
    , PeriphRef{..} <- [ instRef ]
    ]

nameNum :: Text -> (Text, Int)
nameNum s
    | T.null t = (v, 0)
    | otherwise = (v, read $ T.unpack $ T.reverse t)
        where (t, u) = T.break (not . isDigit) $ T.reverse s
              v = T.reverse u

