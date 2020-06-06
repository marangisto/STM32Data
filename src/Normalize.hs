{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
module Normalize
    ( PeriphRef(..)
    , PeriphType(..)
    , PeriphInst(..)
    , normalize
    ) where

import NormalSVD ()
import ParseSVD
import Data.Hashable
import Data.List (sortOn, partition)
import Data.List.Extra (groupSort, groupSortOn)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map.Strict as Map
import Control.Arrow (second)
import Utils

data PeriphRef = PeriphRef
    { svd           :: !Text
    , name          :: !Text
    } deriving (Eq, Ord, Show)

data PeriphType = PeriphType
    { typeRef       :: !PeriphRef
    , description   :: !Text
    , groupName     :: !Text
    , registers     :: ![Register]
    } deriving (Show)

data PeriphInst = PeriphInst
    { instRef       :: !PeriphRef
    , baseAddress   :: !Int
    } deriving (Show)

type Index = Map.Map PeriphRef PeriphRef

normalize :: [SVD] -> [(PeriphType, [PeriphInst])]
normalize xs = mergeInstances moreInsts typeInsts
    where (outright, derived) = partition (isOutright . snd) allPerips
          isOutright = isNothing . derivedFrom
          allPerips = [ (name, p) | SVD{..} <- xs, p <- peripherals ]
          canon = groupSortOn (hash . snd) outright
          typeInsts = map periphTypeInsts canon
          moreInsts = map (fromDerived $ index typeInsts) derived

mergeInstances
    :: [(PeriphRef, PeriphInst)]
    -> [(PeriphType, [PeriphInst])]
    -> [(PeriphType, [PeriphInst])]
mergeInstances xs = map (second (sortOn instRef) . f)
    where f (t@PeriphType{..}, ys) = (t, ys ++ g typeRef)
          g typeRef = fromMaybe [] $ lookup typeRef $ groupSort xs

fromDerived :: Index -> (Text, Peripheral) -> (PeriphRef, PeriphInst)
fromDerived index (svd, Peripheral{..}) = (typeRef, PeriphInst{..})
    where instRef = PeriphRef{..}
          typeRef = case Map.lookup periphRef index of
              Just x -> x
              _ -> error $ "failed to derive " <> show instRef
          periphRef = PeriphRef svd $ fromMaybe (error "!!!") derivedFrom

index :: [(PeriphType, [PeriphInst])] -> Index
index = Map.fromList . concatMap f
    where f (PeriphType{..}, xs) = map (g typeRef) xs
          g typeRef PeriphInst{..} = (instRef, typeRef)

periphTypeInsts :: [(Text, Peripheral)] -> (PeriphType, [PeriphInst])
periphTypeInsts xs@((svd, Peripheral{..}):_) = (PeriphType{..}, insts)
    where typeRef = PeriphRef{..}
          insts = map (periphInst typeRef) xs

periphInst :: PeriphRef -> (Text, Peripheral) -> PeriphInst
periphInst typeRef (svd, Peripheral{..}) = PeriphInst{..}
    where instRef = PeriphRef{..}

