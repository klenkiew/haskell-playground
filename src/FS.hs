module FS
  (
    FSObject,
    Directory (Directory), File (File),
    FileSystem, 
    MemoryFileSystem (MemoryFileSystem),
    getFile, getDirectory, newDirectory, newFile,
    rootDirectory, getFSObjectByPath, getObject, fileContent, directoryContent
  ) where

import Data.List (find)
import Data.Strings (strSplit)
import Control.Monad ((>=>))

skip :: Char -> String -> String
skip toSkip string = case string of
  toSkip:rest -> rest

data Directory = Directory { directoryName :: String, directoryContent :: [FSObject] } deriving (Show)
data File = File { fileName :: String, fileContent :: String } deriving (Show)
data FSObject = DirectoryObject Directory | FileObject File deriving (Show);

newDirectory :: String -> [FSObject] -> FSObject
newDirectory name content = DirectoryObject $ Directory name content

newFile :: String -> String -> FSObject
newFile name content = FileObject $ File name content

name :: FSObject -> String
name fsObject = case fsObject of
  DirectoryObject (Directory name _) -> name
  FileObject (File name _) -> name

getDir :: FSObject -> Maybe Directory
getDir (DirectoryObject dir) = Just dir
getDir _ = Nothing

getF :: FSObject -> Maybe File
getF (FileObject file) = Just file
getF _ = Nothing

getFSObject :: Directory -> String -> Maybe FSObject
getFSObject (Directory _ content) fsObjectName = find ((== fsObjectName) . name) content

getFSObjectByPath :: Directory -> String -> Maybe FSObject
getFSObjectByPath dir = getFSObjectByPathInternal (DirectoryObject dir)

getFSObjectByPathInternal :: FSObject -> String -> Maybe FSObject
getFSObjectByPathInternal (DirectoryObject dir) path = getFSObject dir fsObjectName >>= flip getFSObjectByPathInternal restPath
  where (fsObjectName, restPath) = strSplit "/" path
getFSObjectByPathInternal file@FileObject{} "" = Just file
getFSObjectByPathInternal _ _ = Nothing

class FileSystem a where
  getObject :: a -> String -> Maybe FSObject
  getDirectory :: a -> String -> Maybe Directory
  getDirectory fs = getObject fs >=> getDir
  getFile :: a -> String -> Maybe File
  getFile fs = getObject fs >=> getF

newtype MemoryFileSystem = MemoryFileSystem { rootDirectory :: Directory }
instance FileSystem MemoryFileSystem where
  getObject (MemoryFileSystem rootDirectory) = getFSObjectByPath rootDirectory . skip '/'
