-- | Object capability based IO.
--
-- See <https://github.com/zenhack/haskell-ocap/blob/master/README.md> for
-- an overview.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OCap.IO
    ( OCapIO
    , IOKey
    , getIOKey
    , runOCap
    , runIO
    ) where

import OCap.IO.Internal

-- | The 'OCapIO' monad is like 'IO', except that it does not provide "ambient"
-- authoirty -- in order to perform IO, you also need an 'IOKey'.
newtype OCapIO a = OCapIO (IO a)
    -- TODO: avoid GeneralizedNewtypeDeriving, as it is incompatible with
    -- SafeHaskell.
    deriving
        ( Functor, Applicative, Monad
        , Semigroup, Monoid
        )

-- | Gets an 'IOKey', which can later be used to perform 'IO' actions inside
-- the 'OCapIO' monad.
getIOKey :: IO IOKey
getIOKey = pure IOKey

-- | Run an 'OCapIO' action.
runOCap :: OCapIO a -> IO a
runOCap (OCapIO io) = io

-- | Run an 'IO' action inside of 'CapIO'.
runIO :: IO a -> IOKey -> OCapIO a
-- Note: it is CRITICAL that we force evaluation of the IOKey here;
-- otherwise someone could just pass undefined and circumvent the entirety
-- of the protection provided by OCapIO.
runIO io IOKey = OCapIO io
