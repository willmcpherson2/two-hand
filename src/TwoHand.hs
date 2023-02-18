module TwoHand () where

import Combinators (matchM, plus, satisfyM, star, try, (<<|>>), (|>>))
import Control.Arrow (arr)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
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

data Var
  = Var Name
  | VarNotFound
  deriving (Show, Eq)

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
    <<|>> (fmap VarE <$> try parseVar)
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

parseVar :: Parser String (Maybe Var)
parseVar = do
  name <- parseName
  pure $ Var <$> name

parseName :: Parser String (Maybe Name)
parseName = runMaybeT . MaybeT . plus . try . satisfyM $ \ch ->
  ch `notElem` "()[]{}" && not (isSpace ch)

skipSpace :: Parser String ()
skipSpace = void $ star $ try $ satisfyM isSpace

--------------------------------------------------------------------------------

check :: Program -> Program
check = \case
  Program defs -> Program $ map (checkDef (namesInDefs defs)) defs
  program -> program

namesInDefs :: [Def] -> [Name]
namesInDefs = mapMaybe $ \case
  Def name _ -> Just name
  _ -> Nothing

checkDef :: [Name] -> Def -> Def
checkDef names = \case
  Def name exp -> Def name (checkExp names exp)
  def -> def

checkExp :: [Name] -> Exp -> Exp
checkExp names = \case
  FunE fun -> FunE (checkFun names fun)
  AppE app -> AppE (checkApp names app)
  VarE var -> VarE (checkVar names var)
  exp -> exp

checkFun :: [Name] -> Fun -> Fun
checkFun names = \case
  Fun param body -> Fun param (checkExp (param : names) body)
  fun -> fun

checkApp :: [Name] -> App -> App
checkApp names = \case
  App l r -> App (checkExp names l) (checkExp names r)
  app -> app

checkVar :: [Name] -> Var -> Var
checkVar names = \case
  Var name ->
    if name `elem` names
      then Var name
      else VarNotFound
  var -> var
