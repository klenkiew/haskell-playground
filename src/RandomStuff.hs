module RandomStuff
    ( doWork
    ) where

import qualified Data.Foldable as F  

doWork :: IO ()
doWork = do
            line <- getLine
            let number = read line :: Int
            putStrLn $ "Value for Fibonnaci number  " ++ show number ++ " is " ++ show (fib number)

fib :: Integral a => a -> a
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--main :: IO ()
--main = do
--  gen <- getStdGen
--  gen2 <- getStdGen
--  let 
--    (randomNum, genA) = randomR (0, 100) gen
--    (randomNum', genB) = randomR (0, 100) genA
--  print randomNum
--  print randomNum'
