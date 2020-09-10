module Main (main) where

import Control.Exception (SomeException, try)
import OCap.IO
import OCap.IO.Unsafe

main :: IO ()
main = do
    -- Make sure that:
    --
    -- * Undefined can't be used as an IOKey
    -- * getIOKey and unsafeIOKey both can.
    Left _ <- tryRun undefined
    Right () <- tryRun unsafeIOKey
    Right () <- getIOKey >>= tryRun
    pure ()

tryRun :: IOKey -> IO (Either SomeException ())
tryRun key = try $ runOCap $ runIO (pure ()) key
