module TwoHand
  ( lex,
    parse,
    check,
    eval,
    collect,
    diagnose,
    Source,
    SourceOf (..),
    Display (..),
    Err (..),
    Tree (..),
    Program (..),
    Def (..),
    Exp (..),
    Fun (..),
    App (..),
    Var (..),
    Name (..),
  )
where

import Combinators (endWith, matchM, plus, satisfyM, star, try, (<<|>>))
import Control.Arrow (arr)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Char (isSpace)
import Data.List.Extra (firstJust)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (fromJust, mapMaybe)
import Parser (Parser)
import qualified Parser as P
import Prelude hiding (lex)

type Source = String

class SourceOf a where
  sourceOf :: a -> Source

--------------------------------------------------------------------------------

data Err
  = ParensNoClose Source
  | BracketsNoClose Source
  | BracesNoClose Source
  | ProgramNoMain Source
  | VarNotFound Source
  | NoHands Source
  | OneHand Source
  | TooManyHands Source
  | DefExpected Source
  | NameExpected Source
  | ExpExpected Source
  deriving (Show)

instance SourceOf Err where
  sourceOf = \case
    ParensNoClose s -> s
    BracketsNoClose s -> s
    BracesNoClose s -> s
    ProgramNoMain s -> s
    VarNotFound s -> s
    NoHands s -> s
    OneHand s -> s
    TooManyHands s -> s
    DefExpected s -> s
    NameExpected s -> s
    ExpExpected s -> s

--------------------------------------------------------------------------------

data Name
  = Name Source (NonEmpty Char)
  | NameErr Err
  deriving (Show)

instance Eq Name where
  Name _ a == Name _ b = a == b
  _ == _ = False

instance SourceOf Name where
  sourceOf = \case
    Name s _ -> s
    NameErr err -> sourceOf err

--------------------------------------------------------------------------------

data Tree
  = Parens Source [Tree]
  | Brackets Source [Tree]
  | Braces Source [Tree]
  | TreeName Name
  | TreeErr Err
  deriving (Show)

instance SourceOf Tree where
  sourceOf = \case
    Parens s _ -> s
    Brackets s _ -> s
    Braces s _ -> s
    TreeName name -> sourceOf name
    TreeErr err -> sourceOf err

--------------------------------------------------------------------------------

lex :: String -> [Tree]
lex = P.parse lexTrees

lexTrees :: Parser String [Tree]
lexTrees = star lexTree

lexTree :: Parser String (Maybe Tree)
lexTree = do
  skipSpaces
  lexParens
    <<|>> lexBrackets
    <<|>> lexBraces
    <<|>> lexWord

skipSpaces :: Parser String ()
skipSpaces = void . star . try $ satisfyM isSpace

lexParens :: Parser String (Maybe Tree)
lexParens = mkLexer '(' ')' Parens ParensNoClose

lexBrackets :: Parser String (Maybe Tree)
lexBrackets = mkLexer '[' ']' Brackets BracketsNoClose

lexBraces :: Parser String (Maybe Tree)
lexBraces = mkLexer '{' '}' Braces BracesNoClose

lexWord :: Parser String (Maybe Tree)
lexWord = runMaybeT $ do
  s <- lift $ arr id
  name <- MaybeT . plus . try . satisfyM $ \ch ->
    ch `notElem` "()[]{}" && not (isSpace ch)
  pure $ TreeName $ Name s name

mkLexer ::
  Char ->
  Char ->
  (Source -> [Tree] -> Tree) ->
  (Source -> Err) ->
  Parser String (Maybe Tree)
mkLexer open close ok err = runMaybeT $ do
  s <- lift $ arr id
  MaybeT $ try $ matchM open
  lift $ do
    lexTree `endWith` matchM close >>= \case
      Nothing -> pure $ TreeErr $ err s
      Just inner -> pure $ ok s inner

--------------------------------------------------------------------------------

data Program
  = Program Source [Def]
  | ProgramResult Exp
  | ProgramErr Err
  deriving (Show)

data Def
  = Def Source Name Exp
  | DefErr Err
  deriving (Show)

data Exp
  = FunE Fun
  | AppE App
  | VarE Var
  | ExpErr Err
  deriving (Show)

data Fun
  = Fun Source Name Exp
  | FunErr Err
  deriving (Show)

data App
  = App Source Exp Exp
  | AppErr Err
  deriving (Show)

data Var
  = Var Name
  | VarErr Err
  deriving (Show)

--------------------------------------------------------------------------------

parse :: [Tree] -> Program
parse trees = Program "" (map parseDef trees)

parseDef :: Tree -> Def
parseDef = \case
  Braces s trees -> case trees of
    [] -> DefErr $ NoHands s
    [_] -> DefErr $ OneHand s
    [name, exp] -> Def s (parseName name) (parseExp exp)
    _ -> DefErr $ TooManyHands s
  tree -> DefErr $ DefExpected (sourceOf tree)

parseName :: Tree -> Name
parseName = \case
  TreeName name -> name
  tree -> NameErr $ NameExpected (sourceOf tree)

parseExp :: Tree -> Exp
parseExp = \case
  Parens s trees -> AppE $ case trees of
    [] -> AppErr $ NoHands s
    [_] -> AppErr $ OneHand s
    [l, r] -> App s (parseExp l) (parseExp r)
    _ -> AppErr $ TooManyHands s
  Brackets s trees -> FunE $ case trees of
    [] -> FunErr $ NoHands s
    [_] -> FunErr $ OneHand s
    [param, body] -> Fun s (parseName param) (parseExp body)
    _ -> FunErr $ TooManyHands s
  TreeName name -> VarE $ Var name
  tree -> ExpErr $ ExpExpected (sourceOf tree)

--------------------------------------------------------------------------------

check :: Program -> Program
check = \case
  Program s defs -> Program s (map (checkDef $ namesInDefs defs) defs)
  program -> program

namesInDefs :: [Def] -> [Name]
namesInDefs = mapMaybe $ \case
  Def _ name _ -> Just name
  _ -> Nothing

checkDef :: [Name] -> Def -> Def
checkDef names = \case
  Def s name exp -> Def s name (checkExp names exp)
  def -> def

checkExp :: [Name] -> Exp -> Exp
checkExp names = \case
  FunE fun -> FunE (checkFun names fun)
  AppE app -> AppE (checkApp names app)
  VarE var -> VarE (checkVar names var)
  exp -> exp

checkFun :: [Name] -> Fun -> Fun
checkFun names = \case
  Fun s param body -> Fun s param (checkExp (param : names) body)
  fun -> fun

checkApp :: [Name] -> App -> App
checkApp names = \case
  App s l r -> App s (checkExp names l) (checkExp names r)
  app -> app

checkVar :: [Name] -> Var -> Var
checkVar names = \case
  Var name ->
    if name `elem` names
      then Var name
      else VarErr $ VarNotFound (sourceOf name)
  var -> var

--------------------------------------------------------------------------------

eval :: Program -> Program
eval = \case
  Program s defs -> case resolve ('m' :| "ain") defs of
    Just exp -> ProgramResult $ evalExp defs exp
    Nothing -> ProgramErr $ ProgramNoMain s
  program -> program

evalExp :: [Def] -> Exp -> Exp
evalExp defs exp = case exp of
  AppE (App s l r) -> case l of
    FunE (Fun _ param body) -> evalExp defs (substitute param r body)
    AppE (App s' l' r') ->
      evalExp defs (AppE $ App s (evalExp defs (AppE $ App s' l' r')) r)
    VarE (Var (Name _ name)) ->
      evalExp defs (AppE $ App s (fromJust $ resolve name defs) r)
    _ -> exp
  VarE (Var (Name _ name)) -> evalExp defs (fromJust $ resolve name defs)
  _ -> exp

substitute :: Name -> Exp -> Exp -> Exp
substitute param value body = case body of
  FunE (Fun s param' body')
    | param /= param' -> FunE $ Fun s param' (substitute param value body')
  AppE (App s l r) ->
    AppE $ App s (substitute param value l) (substitute param value r)
  VarE (Var name) | param == name -> value
  _ -> body

resolve :: NonEmpty Char -> [Def] -> Maybe Exp
resolve name = firstJust $ \case
  Def _ (Name _ defName) exp | name == defName -> Just exp
  _ -> Nothing

--------------------------------------------------------------------------------

class Display a where
  display :: a -> String

instance Display Err where
  display err = "<" <> show err <> ">"

instance Display [Tree] where
  display = unlines . map display

instance Display Tree where
  display = \case
    Parens _ trees -> "(" <> unwords (map display trees) <> ")"
    Brackets _ trees -> "[" <> unwords (map display trees) <> "]"
    Braces _ trees -> "{" <> unwords (map display trees) <> "}"
    TreeName name -> display name
    TreeErr err -> display err

instance Display Program where
  display = \case
    Program _ defs -> unlines $ map display defs
    ProgramResult exp -> display exp
    ProgramErr err -> display err

instance Display Def where
  display = \case
    Def _ name exp -> "{" <> display name <> " " <> display exp <> "}"
    DefErr err -> display err

instance Display Exp where
  display = \case
    FunE fun -> display fun
    AppE app -> display app
    VarE var -> display var
    ExpErr err -> display err

instance Display Fun where
  display = \case
    Fun _ param body -> "[" <> display param <> " " <> display body <> "]"
    FunErr err -> display err

instance Display App where
  display = \case
    App _ l r -> "(" <> display l <> " " <> display r <> ")"
    AppErr err -> display err

instance Display Var where
  display = \case
    Var name -> display name
    VarErr err -> display err

instance Display Name where
  display = \case
    Name _ name -> toList name
    NameErr err -> display err

--------------------------------------------------------------------------------

collect :: Program -> [Err]
collect = \case
  Program _ defs -> concatMap collectDef defs
  ProgramErr err -> [err]
  _ -> []

collectDef :: Def -> [Err]
collectDef = \case
  Def _ name exp -> collectName name <> collectExp exp
  DefErr err -> [err]

collectExp :: Exp -> [Err]
collectExp = \case
  FunE fun -> case fun of
    Fun _ param body -> collectName param <> collectExp body
    FunErr err -> [err]
  AppE app -> case app of
    App _ l r -> collectExp l <> collectExp r
    AppErr err -> [err]
  VarE var -> case var of
    Var name -> collectName name
    VarErr err -> [err]
  ExpErr err -> [err]

collectName :: Name -> [Err]
collectName = \case
  NameErr err -> [err]
  _ -> []

--------------------------------------------------------------------------------

diagnose :: Err -> String
diagnose = \case
  ParensNoClose s -> mk s "parenthesis not closed"
  BracketsNoClose s -> mk s "bracket not closed"
  BracesNoClose s -> mk s "brace not closed"
  ProgramNoMain _ -> "warning: program has no main, nothing to evaluate"
  VarNotFound s -> mk s "variable not defined"
  NoHands s -> mk s "no hands"
  OneHand s -> mk s "one hand"
  TooManyHands s -> mk s "too many hands"
  DefExpected s -> mk s "expected definition"
  NameExpected s -> mk s "expected identifier"
  ExpExpected s -> mk s "expected expression"
  where
    mk s msg = "error: " <> msg <> "\n↓\n" <> lineOfSource s

lineOfSource :: Source -> Source
lineOfSource = takeWhile (/= '\n')
