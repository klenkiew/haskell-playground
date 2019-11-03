module Main where

import FS
import Data.Strings (strSplit)

main :: IO ()
main = do
  let fs = MemoryFileSystem { rootDirectory = Directory "root"
    [
      newDirectory "test"
        [
          newFile "testFile" "content"
        ],
      newFile "anotherFile" "some content"
    ]
  }
  let (Just content) = fileContent <$> getFile fs "/test/testFile"
  putStrLn $ "File content: " ++ content
