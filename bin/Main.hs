module Main (main) where

import Data.List (intercalate)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import TwoHand
  ( Display (..),
    Program (..),
    check,
    collect,
    diagnose,
    eval,
    lex,
    parse,
  )
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
  let lexed = lex ("", s)
      parsed = parse lexed
      checked = check parsed
      errs = collect checked
      result = eval checked
  case errs of
    [] -> case result of
      ProgramResult exp -> putStrLn $ display exp
      ProgramErr err -> putStrLn $ diagnose err
      Program _ _ -> undefined
    errs -> putStrLn $ intercalate "\n\n" $ map diagnose errs
