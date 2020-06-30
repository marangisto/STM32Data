{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FrontEnd.Normalize
    ( NormalSVD(..)
    , PeriphType(..)
    , PeriphRef(..)
    , PeriphInst(..)
    , Register(..)
    , Field(..)
    , Interrupt(..)
    , Void
    , normalize
    , peripheralNames
    , nameNum
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
import Utils

data NormalSVD cc pad = NormalSVD
    { family        :: !Text
    , periphTypes   :: ![PeriphType pad]
    , interrupts    :: ![Interrupt]
    , clockControl  :: !cc
    } deriving (Show)

data PeriphType pad = PeriphType
    { typeRef       :: !PeriphRef
    , description   :: !Text
    , groupName     :: !Text
    , registers     :: ![Either pad Register]
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

normalize :: Text -> [SVD] -> NormalSVD () Void
normalize family xs = NormalSVD{..}
    where periphTypes = mergeInstances moreInsts periphTypes'
          interrupts = mergeInterrupts $ concat [ interrupts | SVD{..} <- xs ]
          clockControl = ()
          (outright, derived) = partition (isOutright . snd) allPerips
          isOutright = isNothing . derivedFrom
          canon = groupSortOn (hash . snd) outright
          periphTypes' = map periphType canon
          moreInsts = map (fromDerived $ index periphTypes') derived
          allPerips = [ (name, rename p)
                      | SVD{..} <- xs
                      , p <- peripherals
                      ]

rename :: Peripheral -> Peripheral
rename p@Peripheral{..} = p { name = f name }
    where f "SYSCFG_VREFBUF" = "SYSCFG"
          f s = s

mergeInstances
    :: [(PeriphRef, PeriphInst)]
    -> [PeriphType b]
    -> [PeriphType b]
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

index :: [PeriphType b] -> Index
index = Map.fromList . concatMap f
    where f PeriphType{..} = map (g typeRef) periphInsts
          g typeRef PeriphInst{..} = (instRef, typeRef)

periphType :: [(Text, Peripheral)] -> PeriphType b
periphType xs@((svd, Peripheral{registers=regs,..}):_) = PeriphType{..}
    where registers = map Right regs
          periphInsts = map periphInst xs
          typeRef = PeriphRef{..}
periphType _ = error "expected at least one peripheral"

periphInst :: (Text, Peripheral) -> PeriphInst
periphInst (svd, Peripheral{..}) = PeriphInst{..}
    where instRef = PeriphRef{..}

mergeInterrupts :: [Interrupt] -> [Interrupt]
mergeInterrupts = map f . groupSortOn value
    where f = head . sortOn (Down . T.length . \Interrupt{..} -> name)

peripheralNames :: NormalSVD a b -> [Text]
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

