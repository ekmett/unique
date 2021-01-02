{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

-- | An abstract interface to a concurrent unique symbol generator.
--
-- Unlike @Data.Unique@ from @base@ the values are not a member of 'Ord'. However, there is no global bottleneck.
module Control.Concurrent.Unique
  ( Unique, newUnique
  ) where

import Data.Hashable
import GHC.IO
import GHC.Exts

-- $setup
-- >>> import Data.Hashable

-- | Unique identifiers are created by creating heap objects in kind # that
-- can be compared for value equality and then hashing them using their initial allocation
-- address.
--
-- >>> x <- newUnique
-- >>> y <- newUnique
-- >>> z <- newUnique
--
-- >>> [x == x, y == y, z == z]
-- [True,True,True]
--
-- >>> [x == y, y == z, z == x]
-- [False,False,False]
--
-- The hashes could be same, in theory, but in practice they are different
-- as well.
--
-- >>> [ hash x == hash x, hash y == hash y, hash z == hash z]
-- [True,True,True]
--
-- >>> [ hash x == hash y, hash y == hash z, hash z == hash x]
-- [False,False,False]

-- TODO: If, due to a small heap size we find we have high collision rate on initial allocation location
-- we might consider upgrading this initial hash with something fast and volatile, e.g. rdtsc
data Unique = Unique !Int (MutableByteArray# RealWorld)

instance Eq Unique where
#if MIN_VERSION_base(4,7,0)
  Unique _ p == Unique _ q = isTrue# (sameMutableByteArray# p q)
#else
  Unique _ p == Unique _ q = sameMutableByteArray# p q
#endif

instance Hashable Unique where
  hash (Unique i _) = i
  hashWithSalt d (Unique i _) = hashWithSalt d i

-- | Allocate a new 'Unique' value. The value returned will not compare equal to any other value of type 'Unique' returned by previous calls to 'newUnique'. There is no limit on the number of times 'newUnique' may be called.
newUnique :: IO Unique
newUnique = IO $ \s -> case newByteArray# 0# s of
  (# s', ba #) -> (# s', Unique (I# (addr2Int# (unsafeCoerce# ba))) ba #)
