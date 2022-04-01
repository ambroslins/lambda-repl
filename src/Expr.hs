module Expr where

import Data.Maybe (fromMaybe)

type Name = String

data Expr
  = Variable Name
  | Lambda Name Expr
  | Application Expr Expr
  deriving (Show)

type Env = [(Name, Expr)]

extend :: Env -> Name -> Expr -> Env
extend env name expr = (name, expr) : env

eval :: Env -> Expr -> Expr
eval env expr = case expr of
  Variable var -> fromMaybe expr $ lookup var env
  Lambda arg body -> Lambda arg $ eval (extend env arg (Variable arg)) body
  Application fun arg ->
    case eval env fun of
      Lambda argName body -> eval (extend env argName arg) body
      f -> Application f (eval env arg)

pretty :: Expr -> String
pretty expr = case expr of
  Variable var -> var
  Lambda arg body -> "\\" ++ arg ++ ". " ++ pretty body
  Application fun arg -> parens fun ++ " " ++ pretty arg
  where
    parens e = case e of
      Variable var -> var
      _ -> "(" ++ pretty e ++ ")"
