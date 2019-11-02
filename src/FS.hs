module FS
  (
    FSObject (Directory, File), 
    FileSystem, 
    MemoryFileSystem (MemoryFileSystem), 
    rootDirectory, getFSObjectByPath, getObject, fileContent, directoryContent
  ) where

import Data.List (find)
import Data.Strings (strSplit)
import Debug.Trace (trace)

skip :: Char -> String -> String
--skip (f:rest) toSkip
--  | f == toSkip = rest
skip toSkip string = case string of
  toSkip:rest -> rest

data FSObject = Directory { name :: String, directoryContent :: [FSObject] }
                | File { name :: String, fileContent :: String } deriving (Show)

getFSObject :: FSObject -> String -> Maybe FSObject
--getFSObject file path | trace ("getFSObject " ++ show file ++ " " ++ show path) False = undefined
getFSObject (Directory _ content) fsObjectName = find ((== fsObjectName) . name) content
getFSObject _ _ = Nothing

getFSObjectByPath :: FSObject -> String -> Maybe FSObject
--getFSObjectByPath file path | trace ("getFSObjectByPath " ++ show file ++ " " ++ show path) False = undefined
getFSObjectByPath dir@(Directory _ _) path = getFSObject dir fsObjectName >>= flip getFSObjectByPath restPath
  where (fsObjectName, restPath) = strSplit "/" path
getFSObjectByPath file@File{} "" = Just file
getFSObjectByPath _ _ = Nothing


class FileSystem a where
  getObject :: a -> String -> Maybe FSObject
--  createFile :: a -> String -> a
--  createDirectory :: a -> String -> a

newtype MemoryFileSystem = MemoryFileSystem { rootDirectory :: FSObject }
instance FileSystem MemoryFileSystem where
  getObject (MemoryFileSystem rootDirectory) = getFSObjectByPath rootDirectory . skip '/' 

