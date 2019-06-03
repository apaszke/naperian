{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Naperian where

import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Data.Kind (Type)

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

data Dim (n :: Nat) a where
    Tensor :: (IsTensor a ~ True) => DynamicTensor -> Dim n a
    -- [a] should technically be Vector n a, but I want to play around with
    -- other things first. Fixing this is a TODO.
    Native :: (IsTensor a ~ False) => [a] -> Dim n a

type family IsTensor (a :: Type) :: Bool where
    IsTensor Int = True
    IsTensor (Dim _ a) = IsTensor a
    IsTensor _ = False

instance Show a => Show (Dim n a) where
    show (Native l) = "Native " ++ show l
    show (Tensor l) = "Tensor " ++ show l

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- TFoldable is a bijection between DynamicTensor and more strict types
-- obtained by unfolding a dimension of a DynamicTensor into a Haskell world, or
-- the other way.
--
-- XXX: dunfold is partial, because we cannot do bounds checking on the type level!
class TFoldable a where
    dunfold :: DynamicTensor -> [a]
    dfold :: [a] -> DynamicTensor

instance TFoldable Int where
    dunfold t = map itemInt $ iterateOuter t
    dfold l = tensorConcat $ map Scalar l

instance (IsTensor a ~ True) => TFoldable (Dim n a) where
    dunfold t = map Tensor $ iterateOuter t
    dfold l = tensorConcat $ map (\(Tensor t) -> t) l



data SingIsTensor a where
    SIsTensor   :: (IsTensor a ~ True, TFoldable a) => SingIsTensor a
    SNotATensor :: (IsTensor a ~ False)             => SingIsTensor a


class SingIsTensorI' a (b :: Bool) where
    singIsTensor' :: Proxy b -> SingIsTensor a

instance (TFoldable a, IsTensor a ~ True) => SingIsTensorI' a True where
    singIsTensor' _ = SIsTensor

instance (IsTensor a ~ False) => SingIsTensorI' a False where
    singIsTensor' _ = SNotATensor

class SingIsTensorI a where
    singIsTensor :: SingIsTensor a

instance (IsTensor a ~ b, SingIsTensorI' a b) => SingIsTensorI a where
    singIsTensor = singIsTensor' (Proxy :: Proxy b)

fmap'' :: SingIsTensor a -> SingIsTensor b -> (a -> b) -> Dim n a -> Dim n b
fmap'' (SIsTensor)   (SIsTensor)   f (Tensor t) = Tensor $ dfold    $ fmap f    $ dunfold t
fmap'' (SIsTensor)   (SNotATensor) f (Tensor t) = Native $ fmap f   $ dunfold t
fmap'' (SNotATensor) (SIsTensor)   f (Native l) = Tensor $ dfold    $ fmap f l
fmap'' (SNotATensor) (SNotATensor) f (Native l) = Native $ fmap f l


fmap_ :: (SingIsTensorI a, SingIsTensorI b) => (a -> b) -> Dim n a -> Dim n b
--fmap_ :: (a -> b) -> Dim n a -> Dim n b
fmap_ f x = fmap'' singIsTensor singIsTensor f x

--------------------------------------------------------------------------------
-- Dependent Functor
--------------------------------------------------------------------------------

-- Unfortunately, while one can easily check that (Dim n) is a functor for
-- every n, it is very hard to convince the Haskell typechecker that it is the case.
-- Hence, instead of implementing an instance of (Functor (Dim n)) we define a new
-- type class Functor' f a b, which allows us to adjust the implementation of fmap
-- depending on the types of a and b.
class Functor' f a b where
    fmap' :: (a -> b) -> f a -> f b

-- The following simply enumerates all cases of a and b being a tensor or not, but we have
-- to use the extra level of indirection through DimFunctor' to avoid overlapping instances
-- errors. One can easily convince themself that they _are not_ overlapping, but GHC doesn't
-- care about constraints when picking the class instances and hence we need to resort to those
-- ugly tricks. More about this technique can be found at
-- https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html
instance (IsTensor a ~ at, IsTensor b ~ bt, DimFunctor' f a b at bt) => Functor' f a b where
    fmap' = dimfmap' (Proxy :: Proxy at) (Proxy :: Proxy bt)

class DimFunctor' f a b (at :: Bool) (bt :: Bool) where
    dimfmap' :: Proxy at -> Proxy bt -> (a -> b) -> f a -> f b

instance (IsTensor a ~ False, IsTensor b ~ False) => DimFunctor' (Dim n) a b 'False 'False where
    dimfmap' _ _ f (Native l) = Native $ fmap f l

instance (TFoldable a, IsTensor a ~ True, IsTensor b ~ False) => DimFunctor' (Dim n) a b 'True 'False where
    dimfmap' _ _ f (Tensor t) = Native $ fmap f $ dunfold t

instance (IsTensor a ~ False, IsTensor b ~ True, TFoldable b) => DimFunctor' (Dim n) a b 'False 'True where
    dimfmap' _ _ f (Native l) = Tensor $ dfold $ fmap f l

instance (IsTensor a ~ True, IsTensor b ~ True, TFoldable a, TFoldable b) => DimFunctor' (Dim n) a b 'True 'True where
    dimfmap' _ _ f (Tensor t) = Tensor $ dfold $ fmap f $ dunfold t

--------------------------------------------------------------------------------
-- The Naperian property
--------------------------------------------------------------------------------

-- NB: While it really is the case that we have a Functor' (Dim n) instance for
-- all types a and b, Haskell does not accept my (forall a b. Functor' (Dim n) a b)
-- constraint :(
-- For the same reason I cannot reproduce the `Functor f => Naperian f` part of the
-- paper...
--instance {-# OVERLAPPABLE #-} (forall a b. Functor' f a b) => Functor f where
    --fmap = fmap'

class Naperian f where
    type Index f :: Type
    lookup :: f a -> Index f -> a
    tabulate :: (Index f -> a) -> f a
    positions :: f (Index f)

instance Naperian (Dim n) where
    type Index (Dim n) = Int -- TODO: Use finite naturals
    lookup = undefined
    tabulate = undefined
    positions = undefined

--------------------------------------------------------------------------------
-- Some variables if you want to play around in ghci
--------------------------------------------------------------------------------
x :: Dim 2 (Dim 2 Int)
x = Tensor $ TwoDim [[1, 2], [3, 4]]

y :: Dim 2 Int
y = Tensor $ OneDim [1, 2]

yp2 :: Dim 2 Int
yp2 = fmap' (+2) y
-- *Naperian> yp2
-- Tensor OneDim [3, 4]

xp2 :: Dim 2 (Dim 2 Int)
xp2 = fmap' (fmap' (+2)) x
-- *Naperian> xp2
-- Tensor TwoDim [[3, 4], [5, 6]]

xshow :: Dim 2 String
xshow = fmap' show x
-- NB: Notice how the type seamlessly changes to Native once we get a list of strings!
-- *Naperian> xshow
-- Native ["Tensor OneDim [1,2]","Tensor OneDim [3,4]"]
