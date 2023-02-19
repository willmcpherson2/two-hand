module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import TwoHand (Display (..), check, eval, lex, parse)
import Prelude hiding (lex)

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
  let lexed = lex s
      parsed = parse lexed
      checked = check parsed
      result = eval checked

  putStrLn "source:"
  putStrLn s
  putStrLn ""

  putStrLn "lex:"
  putStrLn $ display lexed
  putStrLn ""

  putStrLn "parse:"
  putStrLn $ display parsed
  putStrLn ""

  putStrLn "check:"
  putStrLn $ display checked
  putStrLn ""

  putStrLn "eval:"
  putStrLn $ display result
