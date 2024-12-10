{-# LANGUAGE UndecidableInstances #-}

{- |
Module: TREXIO.CooArray
Description: Coordinate list sparse array representation for TREXIO
Copyright: Phillip Seeber 2024
License: BSD-3-Clause
Maintainer: phillip.seeber@uni-jena.de
Stability: experimental
Portability: POSIX
-}
module TREXIO.CooArray (
  CooArray,
  values,
  coords,
  cooSize,
  mkCooArrayF,
  mkCooArray,
)
where

import Data.Foldable
import Data.Massiv.Array as Massiv hiding (all, toList)
import GHC.Generics (Generic)

-- | A coordinate list array representation.
data CooArray r ix a = CooArray
  { values_ :: Vector r a
  , coords_ :: Vector r ix
  , cooSize_ :: Sz ix
  -- ^ Size of the COO array
  }
  deriving (Generic)

instance (Eq (Vector r a), Eq (Vector r ix), Eq ix) => Eq (CooArray r ix a) where
  CooArray v1 c1 s1 == CooArray v2 c2 s2 = v1 == v2 && c1 == c2 && s1 == s2

instance (Ord (Vector r a), Ord (Vector r ix), Ord ix) => Ord (CooArray r ix a) where
  compare (CooArray v1 c1 s1) (CooArray v2 c2 s2) =
    compare v1 v2 <> compare c1 c2 <> compare s1 s2

instance
  (Show (Vector r a), Show (Vector r ix), Index ix, Show ix) =>
  Show (CooArray r ix a)
  where
  show CooArray{..} =
    "CooArray "
      <> show values_
      <> " "
      <> show coords_
      <> " "
      <> show cooSize_

values :: CooArray r ix a -> Vector r a
values CooArray{values_} = values_

coords :: CooArray r ix a -> Vector r ix
coords CooArray{coords_} = coords_

cooSize :: CooArray r ix a -> Sz ix
cooSize CooArray{cooSize_} = cooSize_

-- | Make a 'CooArray' from a list of coordinate-value pairs.
mkCooArrayF ::
  (Foldable f, Index ix, Manifest r a, Manifest r ix, MonadThrow m, Stream r Ix1 ix) =>
  Sz ix ->
  f (ix, a) ->
  m (CooArray r ix a)
mkCooArrayF cooSize_ coo
  | isNull unsafeInds = return CooArray{..}
  | otherwise = throwM $ IndexOutOfBoundsException cooSize_ (shead' unsafeInds)
 where
  arr = Massiv.fromList @B Par . toList $ coo
  values_ = Massiv.compute . Massiv.map snd $ arr
  coords_ = Massiv.compute . Massiv.map fst $ arr
  unsafeInds = Massiv.sfilter (not . isSafeIndex cooSize_) coords_

-- | Make a 'CooArray' from a indices and values.
mkCooArray ::
  (MonadThrow m, Index ix, Size r, Stream r Ix1 ix) =>
  Sz ix ->
  Vector r ix ->
  Vector r a ->
  m (CooArray r ix a)
mkCooArray cooSize_ coords_ values_
  | Massiv.size coords_ /= Massiv.size values_ = throwM $ SizeMismatchException (Massiv.size coords_) (Massiv.size values_)
  | not . isNull $ unsafeInds = throwM $ IndexOutOfBoundsException cooSize_ (shead' unsafeInds)
  | otherwise = return CooArray{..}
 where
  unsafeInds = Massiv.sfilter (not . isSafeIndex cooSize_) coords_