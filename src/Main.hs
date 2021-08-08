module Main where

import Control.Monad.Trans (liftIO)
import System.IO
import Entrypoint

main :: IO ()
main = do
  putStrLn "hello world"
  contents <- readFile "../test2.isil"
  if null contents then
    print ""
  else do
    let result = entrypoint (head contents : tail contents) [] [] 1
    print result
    return ()
  print contents