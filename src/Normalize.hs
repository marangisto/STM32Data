{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
module Normalize
    ( NormalSVD(..)
    , PeriphType(..)
    , PeriphRef(..)
    , PeriphInst(..)
    , Interrupt(..)
    , normalize
    ) where

import ParseSVD
import Data.Hashable
import Data.Ord (Down(..))
import Data.List (sortOn, partition)
import Data.List.Extra (groupSort, groupSortOn)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Arrow (second)
import Utils

data NormalSVD = NormalSVD
    { family        :: !Text
    , periphTypes   :: ![PeriphType]
    , interrupts    :: ![Interrupt]
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

normalize :: Text -> [SVD] -> NormalSVD
normalize family xs
    = NormalSVD family (mergeInstances moreInsts periphTypes)
    $ mergeInterrupts $ concat [ interrupts | SVD{..} <- xs ]
    where (outright, derived) = partition (isOutright . snd) allPerips
          isOutright = isNothing . derivedFrom
          allPerips = [ (name, p) | SVD{..} <- xs, p <- peripherals ]
          canon = groupSortOn (hash . snd) outright
          periphTypes = map periphType canon
          moreInsts = map (fromDerived $ index periphTypes) derived

mergeInstances
    :: [(PeriphRef, PeriphInst)]
    -> [PeriphType]
    -> [PeriphType]
mergeInstances xs = map f
    where f t@PeriphType{..} = t
              { periphInsts = h $ periphInsts ++ g typeRef
              }
          g typeRef = fromMaybe [] $ lookup typeRef $ groupSort xs
          h = sortOn instRef

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

