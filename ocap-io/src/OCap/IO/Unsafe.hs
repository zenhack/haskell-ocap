{-# LANGUAGE Unsafe #-}
-- | Unsafe ambient authority.
--
-- This module provides the ability to execute arbitrary 'IO' actions
-- inside of the 'OCapIO' monad. The module is marked Unsafe, so will
-- not be available if 'SafeHaskell' is in use.
--
-- You should avoid this module if possible.
--
-- This mainly exists as an aid in incrementally porting legacy code
-- that runs in IO; you can get such code to run inside 'OCapIO' by calling:
--
-- @runIO unsafeIOKey oldCode@.
module OCap.IO.Unsafe
    ( unsafeIOKey
    ) where

import OCap.IO.Internal

-- | An unsafe ambiently-available 'IOKey'. Avoid using this if you can;
-- treat it like 'unsafePerformIO'. Instead, use "Capnp.IO.getIOKey".
unsafeIOKey :: IOKey
unsafeIOKey = IOKey
