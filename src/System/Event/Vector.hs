{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

-- | This module is intended to be imported qualified to avoid name
-- clashes with @Prelude@ functions, e.g.
--
-- > import qualified System.Event.Vector as V

module System.Event.Vector
    ( -- * Types
      Vector,

      -- * Creation and elimination
      new,
      empty,
      clear,

      -- * Basic interface
      read,
      unsafeRead,
      write,
      unsafeWrite,
      reserve,
      length,
      capacity,
    ) where

import Data.STRef
import Prelude hiding (length, read)

#if defined(__GLASGOW_HASKELL__)

import GHC.Base (MutableArray#, Int(..), (+#), (==#), newArray#, readArray#,
                 writeArray#)
import GHC.ST (ST(..))

#else
# error not implemented for this compiler
#endif

------------------------------------------------------------------------
-- Types

-- | A mutable, dynamic array type for use in the 'ST' monad.
newtype Vector s a = Vector (STRef s (C s a))

-- The actual array contents.
data C s a = C
    {-# UNPACK #-} !(MutableArray# s a)  -- Elements
    {-# UNPACK #-} !Int                  -- Capacity
    {-# UNPACK #-} !Int                  -- Current length

------------------------------------------------------------------------
-- Creation and elimination

-- | /O(n)/ A 'Vector' with @n@ copies of the given element.
new :: Int -> a -> ST s (Vector s a)
new n initial = do
    c <- newC n initial
    ref <- newSTRef c
    return $ Vector ref
{-# INLINE new #-}

-- | /O(1)/ The empty 'Vector'.
empty :: ST s (Vector s a)
empty = new 64 bottomElem
{-# INLINE empty #-}

-- | /O(1)/ Erase all of the elements.
clear :: Vector s a -> ST s ()
clear (Vector ref) = do
    (C marr# cap _) <- readSTRef ref
    writeSTRef ref (C marr# cap 0)
{-# INLINE clear #-}

------------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ 'Vector' index (subscript) operator, starting from 0.  An
-- invalid index results in a runtime error.
read :: Vector s a -> Int -> ST s a
read (Vector ref) i = do
    c <- readSTRef ref
    check "read" c i unsafeReadC
{-# INLINE read #-}

-- | /O(1)/ 'Vector' index (subscript) operator, starting from 0.  May
-- return garbage or crash on an out-of-bounds access.
unsafeRead :: Vector s a -> Int -> ST s a
unsafeRead (Vector ref) i = do
    c <- readSTRef ref
    unsafeReadC c i
{-# INLINE unsafeRead #-}

-- | /O(1)/ Write a mutable array. An invalid index results in a
-- runtime error.
write :: Vector s a -> Int -> a -> ST s ()
write (Vector ref) i e = do
    c <- readSTRef ref
    check "write" c i unsafeWriteC e
{-# INLINE write #-}

-- | /O(1)/ Unchecked write.  May return garbage or crash on an
-- out-of-bounds access.
unsafeWrite :: Vector s a -> Int -> a -> ST s ()
unsafeWrite (Vector ref) i e = do
    c <- readSTRef ref
    unsafeWriteC c i e
{-# INLINE unsafeWrite #-}

-- | /O(n)/ Insert or erase elements at the end such that the size
-- | becomes @n@.
reserve :: Vector s a -> Int -> ST s ()
reserve (Vector ref) n = do
    c <- readSTRef ref
    reserveC c n
{-# INLINE reserve #-}

-- | /O(1)/ Return the number of elements in a 'Vector'.
length :: Vector s a -> ST s Int
length (Vector ref) = do
    (C _ _ len) <- readSTRef ref
    return len
{-# INLINE length #-}

-- | /O(1)/ Return the maximum capacity of a 'Vector'.
capacity :: Vector s a -> ST s Int
capacity (Vector ref) = do
    (C _ cap _) <- readSTRef ref
    return cap
{-# INLINE capacity #-}

------------------------------------------------------------------------
-- Functions working on 'C'

newC :: Int -> a -> ST s (C s a)
newC n@(I# n#) initial = ST $ \s1# ->
    case newArray# n# initial s1# of
        (# s2#, marr# #) -> (# s2#, C marr# n n #)

unsafeReadC :: C s a -> Int -> ST s a
unsafeReadC (C marr# _ _) (I# i#) = ST $ \s1# ->
    case readArray# marr# i# s1# of
        (# s2#, e #) -> (# s2#, e #)

unsafeWriteC :: C s a -> Int -> a -> ST s ()
unsafeWriteC (C marr# _ _) (I# i#) e = ST $ \s1# ->
    case writeArray# marr# i# e s1# of
        s2# -> (# s2#, () #)

reserveC :: C s a -> Int -> ST s ()
reserveC (C marr# cap (I# len#)) n
    | cap >= n  = return ()
    | otherwise = ST $ \s1# ->
        case newArray# newCap# bottomElem s1# of { (# s2#, marr'# #) ->
        let copy i# s3# | i# ==# len# = s3#
                        | otherwise =
                case readArray# marr# i# s3# of { (# s4#, e #) ->
                case writeArray# marr'# i# e s4# of { s5# ->
                copy (i# +# 1#) s5# }} in
        case copy 0# s2# of { s3# ->
        (# s3#, () #) }}
    where
      !(I# newCap#) = nextCap n

-- TODO: Grow capacity in a way that is efficient given GHC's
-- allocator.
nextCap :: Int -> Int
nextCap = (2 *)

------------------------------------------------------------------------
-- Utilities

bottomElem :: a
bottomElem = error "System.Event.Vector.unsafeRead: undefined array element"
{-# NOINLINE bottomElem #-}

moduleError :: String -> String -> a
moduleError func msg = error ("System.Event.Vector." ++ func ++ ": " ++ msg)
{-# NOINLINE moduleError #-}

check :: String -> C t t1 -> Int -> (C t t1 -> Int -> a) -> a
check func c@(C _ _ len) i f
    | i >= 0 && i < len = f c i
    | otherwise = moduleError func "index out of bounds"
{-# INLINE check #-}
