module Parser where

import Control.Applicative
  ( Alternative (empty, many, some, (<|>)),
    Applicative (liftA2, pure, (<*>)),
  )
import Control.Monad (void)
import qualified Data.Char as Char
import Data.Foldable (asum)
import Data.List.NonEmpty (some1)
import Expr (Expr (..), Input (..), Name)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

parse :: Parser a -> String -> Maybe a
parse p s = fst <$> runParser p s

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (x, rest) <- runParser p s
    pure (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> pure (x, s)
  pf <*> px = Parser $ \s -> do
    (f, rest) <- runParser pf s
    runParser (fmap f px) rest

instance Monad Parser where
  p >>= fp = Parser $ \s -> do
    (x, rest) <- runParser p s
    runParser (fp x) rest

instance Alternative Parser where
  empty = Parser (const Nothing)
  px <|> py = Parser $ \s -> runParser px s <|> runParser py s

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \s -> case s of
  (c : rest) | predicate c -> pure (c, rest)
  _ -> empty

lower :: Parser Char
lower = satisfy Char.isLower

alphanum :: Parser Char
alphanum = satisfy Char.isAlphaNum

any :: Parser Char
any = satisfy (const True)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string "" = pure ""
string (c : cs) = liftA2 (:) (char c) (string cs)

whitespace :: Parser Char
whitespace = satisfy Char.isSpace

space :: Parser ()
space = void $ many whitespace

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser ()
symbol = void . token . string

name :: Parser Name
name = token $ liftA2 (:) lower (many $ alphanum <|> char '-')

variable :: Parser Expr
variable = Variable <$> name

lambda :: Parser Expr
lambda = do
  symbol "\\"
  n <- name
  symbol "."
  b <- expr
  pure $ Lambda n b

application :: Parser Expr
application = Application <$> expr <*> expr

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

term :: Parser Expr
term =
  parens expr
    <|> variable
    <|> lambda

eof :: Parser ()
eof = Parser $ \s -> if null s then pure ((), []) else empty

expr :: Parser Expr
expr = foldl1 Application <$> some1 term

define :: Parser Input
define = do
  var <- name
  symbol "="
  x <- expr
  pure $ Define var x

input :: Parser Input
input =
  asum
    [ define,
      Expr <$> expr,
      Quit <$ symbol ":q",
      Empty <$ empty
    ]
    <* eof