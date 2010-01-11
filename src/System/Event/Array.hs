{-# LANGUAGE CPP #-}

module System.Event.Array
    ( Array,
      empty,
      new,
      length,
      capacity,
      unsafeRead,
      unsafeWrite,
      unsafeLoad,
      ensureCapacity,
      useAsPtr,
      snoc,
      clear,
      forM_
    ) where

import Control.Monad (when)
import Data.IORef
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (length, mapM_)

#define BOUNDS_CHECKING 1

#if defined(BOUNDS_CHECKING)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
#define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then error ("System.Event.Array." ++ (_func_) ++ ": bounds error, index " ++ show (_k_) ++ ", capacity " ++ show (_len_)) else
#else
#define CHECK_BOUNDS(_func_,_len_,_k_)
#endif

-- Invariant: size <= capacity
newtype Array a = Array (IORef (AC a))

-- The actual array content.
data AC a = AC
    !(Ptr a)  -- Elements
    !Int      -- Number of elements (length)
    !Int      -- Maximum number of elements (capacity)

empty :: IO (Array a)
empty = fmap Array (newIORef (AC nullPtr 0 0))

new :: Storable a => Int -> IO (Array a)
new c = do
    es <- mallocArray cap
    fmap Array (newIORef (AC es 0 cap))
  where
    cap = firstPowerOf2 c

length :: Array a -> IO Int
length (Array ref) = do
    AC _ len _ <- readIORef ref
    return len

capacity :: Array a -> IO Int
capacity (Array ref) = do
    AC _ _ cap <- readIORef ref
    return cap

unsafeRead :: Storable a => Array a -> Int -> IO a
unsafeRead (Array ref) ix = do
    AC es _ cap <- readIORef ref
    CHECK_BOUNDS("unsafeRead",cap,ix)
      peekElemOff es ix

unsafeWrite :: Storable a => Array a -> Int -> a -> IO ()
unsafeWrite (Array ref) ix a = do
    ac <- readIORef ref
    unsafeWrite' ac ix a

unsafeWrite' :: Storable a => AC a -> Int -> a -> IO ()
unsafeWrite' (AC es _ cap) ix a = do
    CHECK_BOUNDS("unsafeWrite'",cap,ix)
      pokeElemOff es ix a

unsafeLoad :: Storable a => Array a -> (Ptr a -> Int -> IO Int) -> IO Int
unsafeLoad (Array ref) load = do
    AC es _ cap <- readIORef ref
    len' <- load es cap
    writeIORef ref (AC es len' cap)
    return len'

ensureCapacity :: Storable a => Array a -> Int -> IO ()
ensureCapacity (Array ref) c = do
    ac@(AC _ _ cap) <- readIORef ref
    ac'@(AC _ _ cap') <- ensureCapacity' ac c
    when (cap' /= cap) $
      writeIORef ref ac'

ensureCapacity' :: Storable a => AC a -> Int -> IO (AC a)
ensureCapacity' ac@(AC es len cap) c = do
    if c > cap
      then do
        es' <- reallocArray es cap'
        return (AC es' len cap')
      else
        return ac
  where
    cap' = firstPowerOf2 c

useAsPtr :: Array a -> (Ptr a -> Int -> IO b) -> IO b
useAsPtr (Array ref) f = do
    AC es len _ <- readIORef ref
    f es len

snoc :: Storable a => Array a -> a -> IO ()
snoc (Array ref) e = do
    ac@(AC _ len _) <- readIORef ref
    let len' = len + 1
    ac'@(AC es _ cap) <- ensureCapacity' ac len'
    unsafeWrite' ac' len e
    writeIORef ref (AC es len' cap)

clear :: Storable a => Array a -> IO ()
clear (Array ref) = atomicModifyIORef ref $ \(AC es _ cap) -> (AC es 0 cap,())

forM_ :: Storable a => Array a -> (a -> IO ()) -> IO ()
forM_ ary g = forHack ary g undefined
  where
    forHack :: Storable b => Array b -> (b -> IO ()) -> b -> IO ()
    forHack (Array ref) f dummy = do
      AC es len _ <- readIORef ref
      let size = sizeOf dummy
          offset = len * size
      let loop n
              | n >= offset = return ()
              | otherwise = do
                    f =<< peek (es `plusPtr` n)
                    loop (n + size)
      loop 0

firstPowerOf2 :: Int -> Int
firstPowerOf2 n
    | n <= 0    = 0
    | otherwise = 2^p
  where p = (ceiling . logBase (2 :: Double) . realToFrac) n :: Int
