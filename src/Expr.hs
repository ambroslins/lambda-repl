module Expr where

import Data.Maybe (fromMaybe)

type Name = String

data Expr
  = Variable Name
  | Lambda Name Expr
  | Application Expr Expr
  deriving (Eq, Show)

type Env = [(Name, Expr)]

extend :: Env -> Name -> Expr -> Env
extend env name expr = (name, expr) : env

eval :: Env -> Expr -> Expr
eval env expr = case expr of
  Variable var -> fromMaybe expr $ lookup var env
  Lambda arg body -> Lambda arg $ eval (extend env arg (Variable arg)) body
  Application fun arg ->
    let evalArg = eval env arg
     in case eval env fun of
          Lambda argName body -> eval (extend env argName evalArg) body
          f -> Application f evalArg

-- https://www.haskellforall.com/2020/11/pretty-print-syntax-trees-with-this-one.html
pretty :: Expr -> String
pretty expr = case expr of
  Lambda arg body -> "\\" ++ arg ++ ". " ++ pretty body
  _ -> app expr
  where
    app (Application fun arg) = app fun ++ " " ++ var arg
    app e = var e

    var (Variable v) = v
    var e = "(" ++ pretty e ++ ")"
