module TwoHand () where

import Combinators (matchM, plus, satisfyM, star, try, (<<|>>), (|>>))
import Control.Arrow (arr)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty)
import Parser (Parser, runParser)
import Stream (takeToken)

data Program
  = Program [Def]
  | ProgramExcess String
  deriving (Show)

data Def
  = Def Name Exp
  | DefNoName String
  | DefNoClose String
  deriving (Show)

data Exp
  = FunE Fun
  | AppE App
  | VarE Var
  | ExpNoExp String
  deriving (Show)

data Fun
  = Fun Name Exp
  | FunNoParam String
  | FunNoClose String
  deriving (Show)

data App
  = App Exp Exp
  | AppNoClose String
  deriving (Show)

type Var = Name

type Name = NonEmpty Char

parse :: String -> Program
parse = snd . runParser parseProgram

parseProgram :: Parser String Program
parseProgram = do
  defs <- star $ skipSpace *> parseDef
  s <- arr id
  takeToken >>= \case
    Just {} -> pure $ ProgramExcess s
    Nothing -> pure $ Program defs

parseDef :: Parser String (Maybe Def)
parseDef = runMaybeT $ do
  MaybeT $ try $ matchM '{'
  lift $ do
    skipSpace
    s <- arr id
    parseName >>= \case
      Nothing -> pure $ DefNoName s
      Just name -> do
        skipSpace
        exp <- parseExp
        skipSpace
        s <- arr id
        matchM '}' >>= \case
          Nothing -> pure $ DefNoClose s
          Just {} -> pure $ Def name exp

parseExp :: Parser String Exp
parseExp = do
  skipSpace
  s <- arr id
  (fmap FunE <$> try parseFun)
    <<|>> (fmap AppE <$> try parseApp)
    <<|>> (fmap VarE <$> try parseName)
    |>> pure (ExpNoExp s)

parseFun :: Parser String (Maybe Fun)
parseFun = runMaybeT $ do
  MaybeT . try . matchM $ '['
  lift $ do
    skipSpace
    s <- arr id
    parseName >>= \case
      Nothing -> pure $ FunNoParam s
      Just param -> do
        skipSpace
        body <- parseExp
        skipSpace
        s <- arr id
        matchM ']' >>= \case
          Nothing -> pure $ FunNoClose s
          Just {} -> pure $ Fun param body

parseApp :: Parser String (Maybe App)
parseApp = runMaybeT $ do
  MaybeT . try . matchM $ '('
  lift $ do
    skipSpace
    l <- parseExp
    skipSpace
    r <- parseExp
    skipSpace
    s <- arr id
    matchM ')' >>= \case
      Nothing -> pure $ AppNoClose s
      Just {} -> pure $ App l r

parseName :: Parser String (Maybe Name)
parseName = runMaybeT . MaybeT . plus . try . satisfyM $ \ch ->
  ch `notElem` "()[]{}" && not (isSpace ch)

skipSpace :: Parser String ()
skipSpace = void $ star $ try $ satisfyM isSpace
