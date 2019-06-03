{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Naperian where

import Prelude hiding (lookup)

import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Data.Kind (Type, Constraint)

--------------------------------------------------------------------------------
-- Dummy definitions simulating a dynamic tensor type exposed using FFI
--------------------------------------------------------------------------------

data DynamicTensor = Scalar Int | OneDim [Int] | TwoDim [[Int]]
    deriving Show

-- Returns a list of tensor slices along the outermost dimension
iterateOuter :: DynamicTensor -> [DynamicTensor]
iterateOuter (OneDim l) = map Scalar l
iterateOuter (TwoDim l) = map OneDim l

-- Concatenates a list of tensors into a single one
-- XXX: Note that this is unsafe, because we do not check that the
-- resulting array is in fact square!
tensorConcat :: [DynamicTensor] -> DynamicTensor
tensorConcat l@(h : t) =
    case h of
        (Scalar _) -> OneDim $ map (\(Scalar x) -> x) l
        (OneDim _) -> TwoDim $ map (\(OneDim x) -> x) l

itemInt :: DynamicTensor -> Int
itemInt (Scalar x) = x
itemInt _          = error "itemInt called on a non-scalar tensor!"

dynamicTranspose :: DynamicTensor -> DynamicTensor
dynamicTranspose = error "missing dynamicTranspose"

--------------------------------------------------------------------------------
-- Functorial tensors
--------------------------------------------------------------------------------

-- While tensor operators imported via FFI are fast and convenient, representing
-- them using e.g. Naperian functors would require them to be, as the name
-- suggests, functorial. Unfortunately, since the data is represented by opaque
-- C++ blobs, we cannot allow storage of arbitrary Haskell types, and so there is
-- no easy way to implement this. The following is an attempt to define a data
-- family that represents dependently typed multidimensional arrays that attempt
-- to use the optimized C++ representation for all types that support it.

class IsTensor a where
    dunfold :: DynamicTensor -> [a]
    dfold :: [a] -> DynamicTensor

instance IsTensor Int where
    dunfold t = map itemInt $ iterateOuter t
    dfold l = tensorConcat $ map Scalar l

instance (IsTensor a) => IsTensor (Dim n a) where
    dunfold t = map Tensor $ iterateOuter t
    dfold l = tensorConcat $ map (\(Tensor t) -> t) l

data Dim (n :: Nat) a where
    Tensor :: (IsTensor a) => DynamicTensor -> Dim n a
    -- [a] should technically be Vector n a, but I want to play around with
    -- other things first. Fixing this is a TODO.
    Native :: [a] -> Dim n a

instance Show a => Show (Dim n a) where
    show (Native l) = "Native " ++ show l
    show (Tensor l) = "Tensor " ++ show l

--------------------------------------------------------------------------------
-- Functor instance
--------------------------------------------------------------------------------

instance Functor (Dim n) where
    fmap f (Tensor t) = Native $ fmap f   $ dunfold t
    fmap f (Native l) = Native $ fmap f l

--------------------------------------------------------------------------------
-- Naperian functor
--------------------------------------------------------------------------------

class Functor f => Naperian f where
    type Index f :: Type
    lookup :: f a -> Index f -> a
    tabulate :: (Index f -> a) -> f a
    positions :: f (Index f)

instance Naperian (Dim n) where
    type Index (Dim n) = Int -- TODO: Use finite naturals
    lookup = error "missing lookup"
    tabulate = error "missing tabulate"
    positions = error "missing positions"

--------------------------------------------------------------------------------
-- Operator implementations
--------------------------------------------------------------------------------

transpose :: (IsTensor a => IsTensor a) => Dim n (Dim m a) -> Dim m (Dim n a)
transpose x@(Native _) = tabulate . fmap tabulate . flip . fmap lookup . lookup $ x
transpose   (Tensor t) = Tensor $ dynamicTranspose t

--------------------------------------------------------------------------------
-- Some variables if you want to play around in ghci
--------------------------------------------------------------------------------

x :: Dim 2 (Dim 2 Int)
x = Tensor $ TwoDim [[1, 2], [3, 4]]

y :: Dim 2 Int
y = Tensor $ OneDim [1, 2]

-- Unfortunately fmap is heavily Native-biased
yp2 :: Dim 2 Int
yp2 = fmap (+2) y
-- *Naperian> yp2
-- Native [3, 4]

xp2 :: Dim 2 (Dim 2 Int)
xp2 = fmap (fmap (+2)) x
-- *Naperian> xp2
-- Native [Native [3, 4], Native [5, 6]]

xshow :: Dim 2 String
xshow = fmap show x
-- *Naperian> xshow
-- Native ["Tensor OneDim [1,2]","Tensor OneDim [3,4]"]

z_dynamic = transpose x
z_naperian = transpose $ fmap (fmap show) x
