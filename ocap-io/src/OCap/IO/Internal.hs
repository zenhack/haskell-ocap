{-# LANGUAGE Safe #-}
module OCap.IO.Internal(IOKey(..)) where

-- | An 'IOKey' is the most powerful object capability -- it lets you run
-- arbitrary 'IO' inside 'OCapIO'. Get one of these with 'getIOKey' and
-- attenuate it with more specific & limited APIs.
data IOKey = IOKey
