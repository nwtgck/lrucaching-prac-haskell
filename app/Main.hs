module Main where

import qualified Data.LruCache.IO as LruCache.IO
import Control.Concurrent

slowMul10 :: (LruCache.IO.LruHandle Int [Int]) -> Int -> IO [Int]
slowMul10 cache num = LruCache.IO.cached cache num $ do
  threadDelay (3 * 1000 * 1000) 
  return (replicate 100000 num)

  -- TODO: Implement
main :: IO ()
main = do
  putStrLn "Start!"
  cache1 <- LruCache.IO.newLruHandle 1000 -- Capacity is number of keys
  res <- slowMul10 cache1 12
  -- print res
  putStrLn "Done12"
  res <- slowMul10 cache1 12
  -- print res
  putStrLn "Done12"
  
  res <- slowMul10 cache1 33
  putStrLn "Done33"  
  -- print res
  res <- slowMul10 cache1 33
  -- print res
  putStrLn "Done33"
  res <- slowMul10 cache1 12
  -- print res
  putStrLn "Done12"
  
