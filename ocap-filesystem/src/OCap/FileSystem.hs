{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe       #-}
module OCap.FileSystem
    ( Path
    , pathFromSegments
    ) where

import Zhp

import OCap.IO

import System.FilePath

newtype Path = Path [String]

pathFromSegments :: [String] -> Maybe Path
pathFromSegments parts = Path <$> go parts where
    go [] = pure []
    go (x : xs)
        | x `elem` ["", ".", ".."] = Nothing
        | or (map isPathSeparator x) = Nothing
        | otherwise = (x :) <$> go xs
