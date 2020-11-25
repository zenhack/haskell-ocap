{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE Safe                  #-}
module OCap.FileSystem
    ( Path
    , pathFromSegments

    , Dir
    , Handle
    , IOMode(..)

    , openFile
    , openBinaryFile
    , hClose

    , hPutStr
    , hPutStrLn
    ) where

import Zhp hiding
    (Handle, IOMode (..), hClose, hPutStr, hPutStrLn, openBinaryFile)

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

data Handle (r :: Bool) (w :: Bool) = Handle
    { handle :: SIO.Handle
    , key    :: !IOKey
    }

data IOMode :: Bool -> Bool -> * where
    ReadMode :: IOMode 'True 'False
    WriteMode :: IOMode 'False 'True
    AppendMode :: IOMode 'False 'True
    ReadWriteMode :: IOMode 'True 'True

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

convertMode :: IOMode r w -> SIO.IOMode
convertMode = \case
    ReadMode      -> SIO.ReadMode
    WriteMode     -> SIO.WriteMode
    AppendMode    -> SIO.AppendMode
    ReadWriteMode -> SIO.ReadWriteMode

openFile :: Dir -> Path -> IOMode r w -> OCapIO (Handle r w)
openFile dir@Dir{key} path mode = do
    handle <- runIO key $ SIO.openFile (resolvePathStr dir path) (convertMode mode)
    pure Handle { handle, key }

openBinaryFile :: Dir -> Path -> IOMode r w -> OCapIO (Handle r w)
openBinaryFile dir@Dir{key} path mode = do
    handle <- runIO key $ SIO.openBinaryFile (resolvePathStr dir path) (convertMode mode)
    pure Handle { handle, key }

hClose :: Handle r w -> OCapIO ()
hClose Handle {key, handle} = runIO key $ SIO.hClose handle

hPutStr :: Handle r 'True -> String -> OCapIO ()
hPutStr Handle {handle, key} = runIO key . SIO.hPutStr handle

hPutStrLn :: Handle r 'True -> String -> OCapIO ()
hPutStrLn Handle {handle, key} = runIO key . SIO.hPutStrLn handle

hPut :: Handle r 'True -> BS.ByteString -> OCapIO ()
hPut Handle {handle, key} = runIO key . BS.hPut handle

hGet :: Handle 'True w -> Int -> OCapIO BS.ByteString
hGet Handle {handle, key} = runIO key . BS.hGet handle

hGetContents :: Handle 'True w -> OCapIO BS.ByteString
hGetContents Handle {handle, key} = runIO key $ BS.hGetContents handle

readFile :: Dir -> Path -> OCapIO BS.ByteString
readFile dir@Dir{key} path = runIO key $ BS.readFile (resolvePathStr dir path)
