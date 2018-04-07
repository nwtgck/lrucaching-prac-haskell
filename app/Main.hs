module Main where

import qualified Data.LruCache.IO as LruCache.IO
import Control.Concurrent
import Control.Monad

slowMul10 :: (LruCache.IO.LruHandle Int [Int]) -> Int -> IO [Int]
slowMul10 cache num = LruCache.IO.cached cache num $ do
  threadDelay (3 * 1000 * 1000) 
  return (replicate 100000 num)

simplePrac :: IO ()
simplePrac = do
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
  

lruFib :: (LruCache.IO.LruHandle Integer Integer) -> Integer -> IO Integer
lruFib cache n = LruCache.IO.cached cache n $ do
  if n < 2
    then return 1
    else (+) <$> lruFib cache (n - 2) <*> lruFib cache (n - 1)

  -- TODO: Implement
main :: IO ()
main = do
  cache <- LruCache.IO.newLruHandle 2
  n <- lruFib cache 100000
  print n
  when False $ do
    forM_ [0..100000] $ \i -> do
      n <- lruFib cache i
      print n
