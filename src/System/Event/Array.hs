{-# LANGUAGE CPP #-}

module System.Event.Array
    ( Array,
      new,
      empty,
      unsafeRead,
      unsafeWrite,
      ensureCapacity,
      useAsPtr,
      length,
      snoc,
      mapM_
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
    !Int      -- Size
    !Int      -- Capacity

new :: Storable a => Int -> IO (Array a)
new cap = do ptr <- mallocArray cap
             ref <- newIORef (AC ptr 0 cap)
             return $ Array ref

empty :: IO (Array a)
empty = do ref <- newIORef (AC nullPtr 0 0)
           return $ Array ref

unsafeRead :: Storable a => Array a -> Int -> IO a
unsafeRead (Array ref) ix =
    do AC ptr _ cap <- readIORef ref
       CHECK_BOUNDS("unsafeRead",cap,ix)
           peekElemOff ptr ix

unsafeWrite :: Storable a => Array a -> Int -> a -> IO ()
unsafeWrite (Array ref) ix a =
    do AC ptr _ cap <- readIORef ref
       CHECK_BOUNDS("unsafeWrite",cap,ix)
           pokeElemOff ptr ix a

ensureCapacity :: Storable a => Array a -> Int -> IO ()
ensureCapacity (Array ref) newSize =
    do AC ptr sz cap <- readIORef ref
       when (newSize > cap) $ do
           let cap' = newCap cap
           ptr' <- reallocArray ptr cap'
           writeIORef ref (AC ptr' sz cap')
  where newCap 0      = 64
        newCap oldCap = 2 * oldCap

useAsPtr :: Array a -> (Ptr a -> Int -> IO b) -> IO b
useAsPtr (Array ref) f =
    do AC ptr _ cap <- readIORef ref
       f ptr cap

length :: Array a -> IO Int
length (Array ref) =
    do AC _ sz _ <- readIORef ref
       return sz

snoc :: Storable a => Array a -> a -> IO ()
snoc arr element =
    do sz <- length arr
       ensureCapacity arr (sz + 1)
       unsafeWrite arr sz element

mapM_ :: Storable a => Array a -> (a -> IO ()) -> IO ()
mapM_ (Array ref) f =
    do AC ptr sz _ <- readIORef ref
       let loop n | n == sz = return ()
                  | otherwise = do
               e <- peek (ptr `plusPtr` n)
               f e
       loop 0
