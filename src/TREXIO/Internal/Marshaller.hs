module TREXIO.Internal.Marshaller where

import Control.Exception
import Data.Massiv.Array as Massiv hiding (withMArray)
import Data.Massiv.Array.Unsafe
import Data.Maybe (fromJust)
import Foreign

{- | Passes a mutable 'MArray' to a C function. The array may be deallocated
on C side or modified in place. This funciton is super dangerous. You array may
suddenly be gone!
-}
withMArray :: (Index ix) => MArray s S ix a -> (Ptr b -> IO c) -> IO c
withMArray v f = do
  let (arrFPtr, _arrL) = unsafeMArrayToForeignPtr v
  withForeignPtr arrFPtr $ \arrPtr -> f (castPtr arrPtr)

{- | Pass a 'Massiv.Array' to a C function. This function is safe, but the array
is copied to a new memory location.
-}
withArray :: (Storable a, Index ix) => Array S ix a -> (Ptr b -> IO c) -> IO c
withArray v f = do
  mArr <- thaw v
  withMArray mArr f

{- | Get an 'MArray' from C memory. Haskell and C side use the same memory reference.
Be careful with this function. When the undelying pointer vanishes, the array is nonsense
-}
unsafeToMArray :: (Index ix, Storable a) => Sz ix -> Ptr a -> IO (MArray s S ix a)
unsafeToMArray sz ptr = do
  fPtr <- newForeignPtr_ ptr
  let mArr = unsafeMArrayFromForeignPtr0 fPtr (toLinearSz sz)
  resizeMArrayM sz mArr

{- | Get an 'Array' from C memory. The underlying C array is copied and the
result is safe to use.
-}
peekArray :: (Index ix, Storable a) => Sz ix -> Ptr a -> IO (Array S ix a)
peekArray sz ptr = do
  mArr <- unsafeToMArray sz ptr
  freeze Par mArr

-- | Peek an integer array from C memory and cast it into standard int types.
peekIntArray :: (Index ix, Manifest r Int) => Sz ix -> Ptr Int32 -> IO (Array r ix Int)
peekIntArray sz ptr = do
  mArr <- unsafeToMArray sz (castPtr ptr)
  compute . Massiv.map (fromIntegral :: Int32 -> Int) <$> freeze Par mArr

-- | Peek 2D coordinates from C memory.
peek2DCoords :: (Manifest r Ix2) => Sz1 -> Ptr Int32 -> IO (Vector r Ix2)
peek2DCoords sz ptr = do
  coordsSimple :: Matrix S Int <- peekIntArray (consSz sz 2) ptr
  return . compute . Massiv.map mkIx2 . outerSlices $ coordsSimple
 where
  mkIx2 v = Ix2 (v ! 0) (v ! 1)

-- | Peek 3D coordinates from C memory.
peek3DCoords :: (Manifest r Ix3) => Sz1 -> Ptr Int32 -> IO (Vector r Ix3)
peek3DCoords sz ptr = do
  coordsSimple :: Matrix S Int <- peekIntArray (consSz sz 3) ptr
  return . compute . Massiv.map mkIx3 . outerSlices $ coordsSimple
 where
  mkIx3 v = Ix3 (v ! 0) (v ! 1) (v ! 2)

-- | Peek 4D coordinates from C memory.
peek4DCoords :: (Manifest r Ix4) => Sz1 -> Ptr Int32 -> IO (Vector r Ix4)
peek4DCoords sz ptr = do
  coordsSimple :: Matrix S Int <- peekIntArray (consSz sz 4) ptr
  return . compute . Massiv.map mkIx4 . outerSlices $ coordsSimple
 where
  mkIx4 v = Ix4 (v ! 0) (v ! 1) (v ! 2) (v ! 3)

-- | Peek 5D coordinates from C memory.
peek6DCoords :: (Manifest r (IxN 6)) => Sz1 -> Ptr Int32 -> IO (Vector r (IxN 6))
peek6DCoords sz ptr = do
  coordsSimple :: Matrix S Int <- peekIntArray (consSz sz 6) ptr
  return . compute . Massiv.map mkIx6 . outerSlices $ coordsSimple
 where
  mkIx6 v = (v ! 0) :> (v ! 1) :> (v ! 2) :> (v ! 3) :> (v ! 4) :. (v ! 5)

peek8DCoords :: (Manifest r (IxN 8)) => Sz1 -> Ptr Int32 -> IO (Vector r (IxN 8))
peek8DCoords sz ptr = do
  coordsSimple :: Matrix S Int <- peekIntArray (consSz sz 8) ptr
  return . compute . Massiv.map mkIx8 . outerSlices $ coordsSimple
 where
  mkIx8 v = (v ! 0) :> (v ! 1) :> (v ! 2) :> (v ! 3) :> (v ! 4) :> (v ! 5) :> (v ! 6) :. (v ! 7)

-- | Convert a vector of 'Ix2' to a \( m \times 2 \) matrix.
castCoords2D :: (Manifest r1 Ix2, Manifest r2 Int32) => Vector r1 Ix2 -> Matrix r2 Int32
castCoords2D ixVec =
  compute
    . fromJust
    . stackOuterSlicesM
    . Massiv.map ix2ToRow
    $ ixVec
 where
  ix2ToRow :: Ix2 -> Vector S Int32
  ix2ToRow (Ix2 x y) = fromList Seq [fromIntegral x, fromIntegral y]

-- | Convert a vector of 'Ix3' to a \( m \times 3 \) matrix.
castCoords3D :: (Manifest r1 Ix3, Manifest r2 Int32) => Vector r1 Ix3 -> Matrix r2 Int32
castCoords3D ixVec =
  compute
    . fromJust
    . stackOuterSlicesM
    . Massiv.map ix3ToRow
    $ ixVec
 where
  ix3ToRow :: Ix3 -> Vector S Int32
  ix3ToRow (Ix3 x y z) =
    fromList
      Seq
      [ fromIntegral x
      , fromIntegral y
      , fromIntegral z
      ]

-- | Convert a vector of 'Ix4' to a \( m \times 4 \) matrix.
castCoords4D :: (Manifest r1 Ix4, Manifest r2 Int32) => Vector r1 Ix4 -> Matrix r2 Int32
castCoords4D ixVec =
  compute
    . fromJust
    . stackOuterSlicesM
    . Massiv.map ix4ToRow
    $ ixVec
 where
  ix4ToRow :: Ix4 -> Vector S Int32
  ix4ToRow (Ix4 x y z w) =
    fromList
      Seq
      [ fromIntegral x
      , fromIntegral y
      , fromIntegral z
      , fromIntegral w
      ]

-- | Convert a vector of 'Ix6' to a \( m \times 6 \) matrix.
castCoords6D :: (Manifest r1 (IxN 6), Manifest r2 Int32) => Vector r1 (IxN 6) -> Matrix r2 Int32
castCoords6D ixVec =
  compute
    . fromJust
    . stackOuterSlicesM
    . Massiv.map ix6ToRow
    $ ixVec
 where
  ix6ToRow :: IxN 6 -> Vector S Int32
  ix6ToRow (x :> y :> z :> w :> u :. v) =
    fromList
      Seq
      [ fromIntegral x
      , fromIntegral y
      , fromIntegral z
      , fromIntegral w
      , fromIntegral u
      , fromIntegral v
      ]

castCoords8D :: (Manifest r1 (IxN 8), Manifest r2 Int32) => Vector r1 (IxN 8) -> Matrix r2 Int32
castCoords8D ixVec =
  compute
    . fromJust
    . stackOuterSlicesM
    . Massiv.map ix8ToRow
    $ ixVec
 where
  ix8ToRow :: IxN 8 -> Vector S Int32
  ix8ToRow (x :> y :> z :> w :> u :> v :> t :. s) =
    fromList
      Seq
      [ fromIntegral x
      , fromIntegral y
      , fromIntegral z
      , fromIntegral w
      , fromIntegral u
      , fromIntegral v
      , fromIntegral t
      , fromIntegral s
      ]

-- | Like 'callocArray' but safe with bracket pattern
callocaArray :: (Storable a) => Int -> (Ptr a -> IO b) -> IO b
callocaArray nEl =
  bracket
    (callocArray nEl)
    free
