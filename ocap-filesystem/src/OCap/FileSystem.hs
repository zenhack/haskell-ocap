{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE Safe                  #-}
module OCap.FileSystem
    ( Path
    , pathFromSegments

    , Dir
    , Handle

    , openFile
    , openBinaryFile

    , hPutStr
    , hPutStrLn
    ) where

import Zhp hiding (Handle, hPutStr, hPutStrLn, openBinaryFile)

import qualified Data.ByteString as BS
import qualified System.IO       as SIO

import OCap.IO

import System.FilePath

newtype Path = Path { pathSegments :: [String] }
    deriving(Eq, Ord)

data Dir = Dir
    { dirPath :: Path
    , key     :: !IOKey
    }

data Handle = Handle
    { handle :: SIO.Handle
    , key    :: !IOKey
    }

instance Show Path where
    show (Path segs) = "pathFromSegments " <> show segs

instance Semigroup Path where
    Path x <> Path y = Path (x <> y)

instance Monoid Path where
    mempty = Path []

pathFromSegments :: [String] -> Maybe Path
pathFromSegments parts = Path <$> go parts where
    go [] = pure []
    go (x : xs)
        | x `elem` ["", ".", ".."] = Nothing
        | or (map isPathSeparator x) = Nothing
        | otherwise = (x :) <$> go xs

resolvePathStr :: Dir -> Path -> String
resolvePathStr Dir{dirPath} path =
    let Path {pathSegments} = dirPath <> path in
    concatMap ("/"<>) pathSegments

openFile :: Dir -> Path -> IOMode -> OCapIO Handle
openFile dir@Dir{key} path mode = do
    handle <- runIO key $ SIO.openFile (resolvePathStr dir path) mode
    pure Handle { handle, key }

openBinaryFile :: Dir -> Path -> IOMode -> OCapIO Handle
openBinaryFile dir@Dir{key} path mode = do
    handle <- runIO key $ SIO.openBinaryFile (resolvePathStr dir path) mode
    pure Handle { handle, key }

hPutStr :: Handle -> String -> OCapIO ()
hPutStr Handle {handle, key} = runIO key . SIO.hPutStr handle

hPutStrLn :: Handle -> String -> OCapIO ()
hPutStrLn Handle {handle, key} = runIO key . SIO.hPutStrLn handle

hPut :: Handle -> BS.ByteString -> OCapIO ()
hPut Handle {handle, key} = runIO key . BS.hPut handle

hGet :: Handle -> Int -> OCapIO BS.ByteString
hGet Handle {handle, key} = runIO key . BS.hGet handle
