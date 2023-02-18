module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import TwoHand (check, display, eval, parse)

main :: IO ()
main =
  getArgs >>= \case
    filename : _ -> do
      doesFileExist filename >>= \case
        True -> readFile filename >>= report
        False -> putStrLn "no such file exists"
    _ -> putStrLn "please supply a file"

report :: String -> IO ()
report s = do
  let parsed = parse s
      checked = check parsed
      result = eval checked

  putStrLn "source:"
  putStrLn s
  putStrLn ""

  putStrLn "parse:"
  putStrLn $ display parsed
  putStrLn ""

  putStrLn "check:"
  putStrLn $ display checked
  putStrLn ""

  putStrLn "eval:"
  putStrLn $ display result
