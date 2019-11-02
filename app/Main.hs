module Main where

import FS
import Data.Strings (strSplit)

main :: IO ()
main = do
  let fs = MemoryFileSystem { rootDirectory = Directory "root"
    [
      Directory "test"
        [
          File "testFile" "content"
        ],
      File "anotherFile" "some content"
    ]
  }
  let (Just content) = fileContent <$> getObject fs "/test/testFile"
  putStrLn $ "File content: " ++ content
