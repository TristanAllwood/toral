{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Heap.Heap
( newHeap
, runHeap
, viewHeap
, runNewHeap
, getWorld
, newRef
, readRef
, writeRef
, modifyRefM
, Heap
, Ref
, HeapM
, focusRef
) where

import Control.Category ((.))
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.ST.Strict
import Control.Monad.ST.Unsafe
import Data.Functor
import Data.Maybe
import Data.STRef.Strict
import Data.Vector (Vector, MVector)
import Prelude hiding ((.))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Mutable as MV
import System.IO.Unsafe
import System.Mem.Weak
import Unsafe.Coerce

newtype HeapM h a = HeapM { _runHeapM :: DynHeap h -> ST h a }

instance Applicative (HeapM h) where
  pure x = HeapM $ \_ -> pure x
  (HeapM hf) <*> (HeapM ha) = HeapM $ \d -> do
    f <- hf d
    a <- ha d
    return (f a)

instance Monad (HeapM h) where
  (HeapM x) >>= f = HeapM $ (\a -> (x a) >>= flip _runHeapM a . f )

instance Functor (HeapM h) where
  fmap = liftM

instance MonadFix (HeapM h) where
  -- mfix :: (a -> HeapM h a) -> HeapM h a
  mfix  f = HeapM $ \a -> (mfix (flip _runHeapM a . f))

data Any = forall a . Any !a

data Ref h a where
  Ref     :: !Int -> Ref h a
  Focused :: !(Lens' a b) -> !(Ref h a) -> Ref h b

data Heap (w :: * -> *) = H !(forall h . w h) !(Vector (Weak Any)) ![Int]

data DynHeap s = DH !(STRef s (MVector s (Weak Any))) !(STRef s [Int])

newHeap ::  (forall h . w h) -> (Heap w)
newHeap w = H w V.empty []

runNewHeap :: (forall h . HeapM h (w h)) -> Heap w
runNewHeap = runHeap' V.empty []

viewHeap :: forall w a . Heap w -> (forall h . (forall v . Ref h v -> v) -> w h -> a) -> a
viewHeap (H w v _) f = f grab (unsafeCoerce w)    {- TODO: pretttty sure we can get rid of usc and the forall h on grab -}
  where
    grab :: forall h v . Ref h v -> v
    grab (Ref x) = case unsafePerformIO (deRefWeak (v V.! x)) of
                      Just (Any val) -> unsafeCoerce val
    grab (Focused p r) = (grab r) ^. p

runHeap :: Heap w -> (forall h . w h -> HeapM h (w h)) -> Heap w
runHeap (H w v ints) act = runHeap' v ints (act w)

runHeap' ::forall w . (Vector (Weak Any)) -> [Int] -> (forall h . HeapM h (w h)) -> Heap w
runHeap' v ints act = runST runHeap''
  where
    runHeap'' :: forall s . ST s (Heap w)
    runHeap'' = do
      mv <- GMV.unstream (VG.stream v)
      mvRef <- newSTRef mv
      intsRef <- newSTRef ints

      w' <- _runHeapM act (DH mvRef intsRef)

      mv' <- readSTRef mvRef
      ints' <- readSTRef intsRef
      v' <- VG.unsafeFreeze mv' {- could be create -}
      return (H (unsafeCoerce w') v' ints')

getWorld :: Heap w -> (forall a . w a)
getWorld (H w _ _) = w

newRef :: a -> HeapM h (Ref h a)
newRef !v = HeapM $ \dh@(DH mvRef _) -> do
  cellAddr <- claimNextCell dh
  let newRef = Ref cellAddr
  writeRef' newRef cellAddr v mvRef
  return newRef


readRef :: forall h a . Ref h a -> HeapM h a
readRef (Focused p r) = view p <$> readRef r
readRef (Ref x) = HeapM readRef'
  where
    readRef' :: DynHeap h -> ST h a
    readRef'  (DH mvRef _) = do
      mv <- readSTRef mvRef
      wk <- MV.read mv x
      Just (Any val) <- unsafeIOToST (deRefWeak wk)
      return (unsafeCoerce val)

writeRef :: Ref h a -> a -> HeapM h ()
writeRef r@(Ref x)       v = HeapM $ \(DH mvRef _) -> writeRef' r x v mvRef
writeRef (Focused p r) v = modifyRefM r (return . (p .~ v))

modifyRefM :: Ref h a -> (a -> HeapM h a) -> HeapM h ()
modifyRefM ref f = do
  v <- readRef ref
  v' <- f v
  writeRef ref v'

{- mutates the DynHeap argument -}
claimNextCell :: DynHeap s -> ST s Int
claimNextCell (DH vecRef freesRef) = do
  frees <- readSTRef freesRef

  unlessSupply frees $ do
    vec <- readSTRef vecRef
    newFrees <- freeSpaces vec
    unlessSupply newFrees $ do
      vec' <- MV.grow vec 1
      writeSTRef vecRef vec'
      return (MV.length vec)
  where
    unlessSupply (x:xs) _ = do
      writeSTRef freesRef xs
      return x
    unlessSupply [] act   = act

freeSpaces :: forall s . MVector s (Weak Any) -> ST s [Int]
freeSpaces mv = catMaybes <$> mapM checkOne [0..(MV.length mv - 1)]
  where
    checkOne :: Int -> ST s (Maybe Int)
    checkOne x = do
      !wk <- MV.read mv x
      !v <- unsafeIOToST (deRefWeak wk)
      if (isNothing v)
        then return (Just x)
        else return Nothing

writeRef' :: Ref h a -> Int -> a -> STRef h (MVector h (Weak Any)) -> ST h ()
writeRef' !newRef x v mvRef = do
  wkPtr <- unsafeIOToST (mkWeak newRef (Any v) Nothing)
  mv <- readSTRef mvRef
  MV.write mv x wkPtr

focusRef :: (Lens' a b) -> Ref h a -> Ref h b
focusRef p r@(Ref _)      = Focused p r
focusRef p (Focused p' r) = Focused (p' . p) r
