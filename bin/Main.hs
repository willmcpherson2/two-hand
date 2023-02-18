module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import TwoHand ()

main :: IO ()
main =
  getArgs >>= \case
    filename : _ -> do
      doesFileExist filename >>= \case
        True -> readFile filename >>= putStrLn
        False -> putStrLn "no such file exists"
    _ -> putStrLn "please supply a file"
